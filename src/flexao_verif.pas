program FlexaoVerif;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

var
  // Input variables
  bw, d, dl: Double;
  fck, fyk, es: Double;
  as_given, asl_given: Double;
  gamac, gamas: Double;
  jsonOutput, verboseOutput: Boolean;
  fieldOutput: string;

  // Output variables
  Mrd: Double;              // Design moment capacity (kN.m)
  x: Double;                // Neutral axis depth
  dominio: Integer;         // Strain domain
  verificacao: string;      // Status message

// Subroutine to calculate stress in steel
function CalcularTensaoAco(es, esl, fyd: Double): Double;
var
  ess, eyd, tsl: Double;
begin
  ess := Abs(esl);
  eyd := fyd / es;
  if ess < eyd then
    tsl := es * ess
  else
    tsl := fyd;
  if esl < 0 then
    tsl := -tsl;
  Result := tsl;
end;

procedure ShowUsage;
begin
  WriteLn(StdErr, 'Usage: flexao_verif --bw=VALUE --d=VALUE --As=VALUE --fck=VALUE --fyk=VALUE [OPTIONS]');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Required Parameters:');
  WriteLn(StdErr, '  --bw       Section width (cm)');
  WriteLn(StdErr, '  --d        Effective depth (cm)');
  WriteLn(StdErr, '  --As       Tensile reinforcement area (cm²)');
  WriteLn(StdErr, '  --fck      Characteristic concrete strength (MPa)');
  WriteLn(StdErr, '  --fyk      Characteristic steel yield strength (MPa)');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Optional Parameters:');
  WriteLn(StdErr, '  --Asl      Compression reinforcement area (cm²), default: 0');
  WriteLn(StdErr, '  --dl       Distance to compression steel (cm), default: 0.1*d');
  WriteLn(StdErr, '  --es       Steel elastic modulus (GPa), default: 200');
  WriteLn(StdErr, '  --gamac    Concrete safety factor, default: 1.4');
  WriteLn(StdErr, '  --gamas    Steel safety factor, default: 1.15');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Output Formats:');
  WriteLn(StdErr, '  (default)  Concise output for AI agents: Mrd=X.XX x=X.XX dominio=N');
  WriteLn(StdErr, '  --verbose  Human-readable formatted output');
  WriteLn(StdErr, '  --json     JSON format for structured parsing');
  WriteLn(StdErr, '  --field=X  Output only specific field');
  WriteLn(StdErr, '             Valid fields: Mrd, x, dominio, verificacao');
  Halt(2);
end;

function GetParamValue(const name: string): string;
var
  i: Integer;
  prefix: string;
begin
  prefix := '--' + name + '=';
  Result := '';
  for i := 1 to ParamCount do
  begin
    if Pos(prefix, ParamStr(i)) = 1 then
    begin
      Result := Copy(ParamStr(i), Length(prefix) + 1, MaxInt);
      Exit;
    end;
  end;
end;

function HasFlag(const name: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to ParamCount do
    if ParamStr(i) = '--' + name then
      Exit(True);
end;

procedure ParseArguments;
var
  s: string;
begin
  if HasFlag('help') or (ParamCount = 0) then
    ShowUsage;

  jsonOutput := HasFlag('json');
  verboseOutput := HasFlag('verbose');
  fieldOutput := GetParamValue('field');

  // Required parameters
  s := GetParamValue('bw');
  if s = '' then ShowUsage;
  bw := StrToFloat(s);

  s := GetParamValue('d');
  if s = '' then ShowUsage;
  d := StrToFloat(s);

  s := GetParamValue('As');
  if s = '' then ShowUsage;
  as_given := StrToFloat(s);

  s := GetParamValue('fck');
  if s = '' then ShowUsage;
  fck := StrToFloat(s);

  s := GetParamValue('fyk');
  if s = '' then ShowUsage;
  fyk := StrToFloat(s);

  // Optional parameters
  s := GetParamValue('Asl');
  if s = '' then
    asl_given := 0
  else
    asl_given := StrToFloat(s);

  s := GetParamValue('dl');
  if s = '' then
    dl := 0.1 * d
  else
    dl := StrToFloat(s);

  s := GetParamValue('es');
  if s = '' then
    es := 200.0
  else
    es := StrToFloat(s);

  s := GetParamValue('gamac');
  if s = '' then
    gamac := 1.4
  else
    gamac := StrToFloat(s);

  s := GetParamValue('gamas');
  if s = '' then
    gamas := 1.15
  else
    gamas := StrToFloat(s);
end;

procedure Calculate;
var
  alamb, alfac, eu: Double;
  fcd, tcd, fyd: Double;
  xi, xf, xk, fi, ff, fk: Double;
  rc, zc: Double;
  esl, tsl: Double;
  c, xl: Double;
  prod: Double;
  delta: Double;
begin
  // Rectangular stress block parameters
  if fck <= 50 then
  begin
    alamb := 0.8;
    alfac := 0.85;
    eu := 3.5;
  end
  else
  begin
    alamb := 0.8 - (fck - 50) / 400;
    alfac := 0.85 * (1 - (fck - 50) / 200);
    eu := 2.6 + 35 * Power((90 - fck) / 100, 4);
  end;

  // Unit conversion
  fck := fck / 10;  // MPa to kN/cm²
  fyk := fyk / 10;
  es := 100 * es;   // GPa to kN/cm²

  // Design resistances
  fcd := fck / gamac;
  tcd := alfac * fcd;
  fyd := fyk / gamas;
  delta := dl / d;

  // Bisection method to find neutral axis
  // Initial value
  xi := 0.01;
  // Final value
  xf := d;

  // Function evaluation at xi
  xl := eu * d / (eu + 10);
  if xi <= xl then
    c := 0.01 / (d - xi)
  else
    c := eu / (1000 * xi);

  rc := alamb * bw * xi * tcd;
  fi := rc;
  // Tensile steel contribution
  esl := c * (xi - d);
  tsl := CalcularTensaoAco(es, esl, fyd);
  fi := fi + as_given * tsl;
  // Compression steel contribution
  if asl_given > 0 then
  begin
    esl := c * (xi - dl);
    tsl := CalcularTensaoAco(es, esl, fyd);
    fi := fi + asl_given * tsl;
  end;
  fi := fi / (bw * d * tcd);

  // Function evaluation at xf
  if xf <= xl then
    c := 0.01 / (d - xf)
  else
    c := eu / (1000 * xf);

  rc := alamb * bw * xf * tcd;
  ff := rc;
  esl := c * (xf - d);
  tsl := CalcularTensaoAco(es, esl, fyd);
  ff := ff + as_given * tsl;
  if asl_given > 0 then
  begin
    esl := c * (xf - dl);
    tsl := CalcularTensaoAco(es, esl, fyd);
    ff := ff + asl_given * tsl;
  end;
  ff := ff / (bw * d * tcd);

  // Bisection iteration
  fk := 1;
  while Abs(fk) > 0.001 do
  begin
    xk := (xi * ff - xf * fi) / (ff - fi);

    // Function evaluation at xk
    if xk <= xl then
      c := 0.01 / (d - xk)
    else
      c := eu / (1000 * xk);

    rc := alamb * bw * xk * tcd;
    fk := rc;
    esl := c * (xk - d);
    tsl := CalcularTensaoAco(es, esl, fyd);
    fk := fk + as_given * tsl;
    if asl_given > 0 then
    begin
      esl := c * (xk - dl);
      tsl := CalcularTensaoAco(es, esl, fyd);
      fk := fk + asl_given * tsl;
    end;
    fk := fk / (bw * d * tcd);

    prod := fk * fi;
    if prod > 0 then
    begin
      xi := xk;
      fi := fk;
    end
    else
    begin
      xf := xk;
      ff := fk;
    end;
  end;

  // Convergence reached
  x := xk;

  // Calculate moment capacity
  zc := d - 0.5 * alamb * x;
  Mrd := rc * zc;

  // Add contribution from reinforcement
  esl := c * (x - d);
  tsl := CalcularTensaoAco(es, esl, fyd);
  Mrd := Mrd + as_given * tsl * (d - d);  // Always zero for tensile steel at d

  if asl_given > 0 then
  begin
    esl := c * (x - dl);
    tsl := CalcularTensaoAco(es, esl, fyd);
    Mrd := Mrd + asl_given * tsl * (d - dl);
  end;

  // Convert to kN.m
  Mrd := Mrd / 100;

  // Determine domain
  xl := eu * d / (eu + 10);
  if x <= xl then
    dominio := 2
  else if x <= 0.8 * d then
    dominio := 3
  else
    dominio := 4;

  verificacao := 'OK';
end;

procedure OutputResults;
var
  verif_suffix: string;
begin
  // Field-specific output
  if fieldOutput <> '' then
  begin
    if (fieldOutput = 'Mrd') or (fieldOutput = 'mrd') then
      WriteLn(Mrd:0:2)
    else if (fieldOutput = 'x') then
      WriteLn(x:0:2)
    else if (fieldOutput = 'dominio') then
      WriteLn(dominio)
    else if (fieldOutput = 'verificacao') then
      WriteLn(verificacao)
    else
    begin
      WriteLn(StdErr, 'Error: Unknown field "', fieldOutput, '"');
      WriteLn(StdErr, 'Valid fields: Mrd, x, dominio, verificacao');
      Halt(2);
    end;
    Exit;
  end;

  // JSON output
  if jsonOutput then
  begin
    WriteLn('{');
    WriteLn('  "Mrd": ', Mrd:0:2, ',');
    WriteLn('  "x": ', x:0:2, ',');
    WriteLn('  "dominio": ', dominio, ',');
    WriteLn('  "verificacao": "', verificacao, '"');
    WriteLn('}');
    Exit;
  end;

  // Verbose output
  if verboseOutput then
  begin
    WriteLn('============================================');
    WriteLn('  VERIFICACAO FLEXAO - NBR 6118');
    WriteLn('============================================');
    WriteLn('');
    WriteLn('Momento resistente (Mrd):    ', Mrd:8:2, ' kNm');
    WriteLn('Linha neutra (x):            ', x:8:2, ' cm');
    WriteLn('Dominio:                     ', dominio);
    WriteLn('');
    WriteLn('Verificacao: ', verificacao);
    WriteLn('============================================');
    Exit;
  end;

  // Default: Concise output
  Write('Mrd=', Mrd:0:2);
  Write(' x=', x:0:2);
  Write(' dominio=', dominio);

  if verificacao <> 'OK' then
  begin
    verif_suffix := StringReplace(verificacao, ' ', '_', [rfReplaceAll]);
    Write(' [', verif_suffix, ']');
  end;

  WriteLn;
end;

begin
  // Force dot as decimal separator for international compatibility
  FormatSettings.DecimalSeparator := '.';

  try
    ParseArguments;
    Calculate;
    OutputResults;
    Halt(0);
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'Error: ', E.Message);
      Halt(1);
    end;
  end;
end.
