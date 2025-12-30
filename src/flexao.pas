program Flexao;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

var
  // Input variables
  bw, h, d, dl: Double;
  fck, fyk, es: Double;
  mk: Double;
  gamac, gamas, gamaf, bduct: Double;
  jsonOutput, verboseOutput: Boolean;
  fieldOutput: string;

  // Output variables
  As_trac, As_comp: Double;  // Areas of tensile and compression reinforcement
  ami, amilim: Double;       // Reduced moments
  dominio: Integer;          // Strain domain
  taxa: Double;              // Reinforcement ratio
  verificacao: string;       // Status message

// Subroutine to calculate stress in steel
function CalcularTensaoAco(es, esl, fyd: Double): Double;
var
  ess, eyd, tsl: Double;
begin
  // Working with positive strain
  ess := Abs(esl);
  eyd := fyd / es;

  if ess < eyd then
    tsl := es * ess
  else
    tsl := fyd;

  // Change sign if necessary
  if esl < 0 then
    tsl := -tsl;

  Result := tsl;
end;

procedure ShowUsage;
begin
  WriteLn(StdErr, 'Usage: flexao --bw=VALUE --h=VALUE --d=VALUE --fck=VALUE --fyk=VALUE --mk=VALUE [OPTIONS]');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Required Parameters:');
  WriteLn(StdErr, '  --bw       Section width (cm)');
  WriteLn(StdErr, '  --h        Section height (cm)');
  WriteLn(StdErr, '  --d        Effective depth (cm)');
  WriteLn(StdErr, '  --fck      Characteristic concrete strength (MPa)');
  WriteLn(StdErr, '  --fyk      Characteristic steel yield strength (MPa)');
  WriteLn(StdErr, '  --mk       Characteristic bending moment (kN.m)');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Optional Parameters:');
  WriteLn(StdErr, '  --dl       Distance to compression steel (cm), default: 0.1*d');
  WriteLn(StdErr, '  --es       Steel elastic modulus (GPa), default: 200');
  WriteLn(StdErr, '  --gamac    Concrete safety factor, default: 1.4');
  WriteLn(StdErr, '  --gamas    Steel safety factor, default: 1.15');
  WriteLn(StdErr, '  --gamaf    Load safety factor, default: 1.4');
  WriteLn(StdErr, '  --bduct    Ductility coefficient, default: 1.0');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Output Formats:');
  WriteLn(StdErr, '  (default)  Concise output for AI agents: As=X.XX As''=X.XX dominio=N rho=X.XX');
  WriteLn(StdErr, '  --verbose  Human-readable formatted output');
  WriteLn(StdErr, '  --json     JSON format for structured parsing');
  WriteLn(StdErr, '  --field=X  Output only specific field');
  WriteLn(StdErr, '             Valid fields: As, As_comp, dominio, taxa, ami, amilim, verificacao');
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

  s := GetParamValue('h');
  if s = '' then ShowUsage;
  h := StrToFloat(s);

  s := GetParamValue('d');
  if s = '' then ShowUsage;
  d := StrToFloat(s);

  s := GetParamValue('fck');
  if s = '' then ShowUsage;
  fck := StrToFloat(s);

  s := GetParamValue('fyk');
  if s = '' then ShowUsage;
  fyk := StrToFloat(s);

  s := GetParamValue('mk');
  if s = '' then ShowUsage;
  mk := StrToFloat(s);

  // Optional parameters with defaults
  s := GetParamValue('dl');
  if s = '' then
    dl := 0.1 * d  // Default: 10% of effective depth
  else
    dl := StrToFloat(s);

  s := GetParamValue('es');
  if s = '' then
    es := 200.0  // GPa
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

  s := GetParamValue('gamaf');
  if s = '' then
    gamaf := 1.4
  else
    gamaf := StrToFloat(s);

  s := GetParamValue('bduct');
  if s = '' then
    bduct := 1.0
  else
    bduct := StrToFloat(s);
end;

procedure Calculate;
var
  alamb, alfac, eu, qlim: Double;
  fcd, tcd, fyd: Double;
  amd, amk: Double;
  delta: Double;
  qsi, qsia: Double;
  esl, tsl: Double;
  romin, asmin: Double;
  a: Double;
  fck_orig, fyd_orig: Double;
begin
  // Store original values for minimum reinforcement calculation
  fck_orig := fck;

  // Parameters of rectangular stress block (based on fck)
  if fck <= 50 then
  begin
    alamb := 0.8;
    alfac := 0.85;
    eu := 3.5;
    qlim := 0.8 * bduct - 0.35;
  end
  else
  begin
    alamb := 0.8 - (fck - 50) / 400;
    alfac := 0.85 * (1 - (fck - 50) / 200);
    eu := 2.6 + 35 * Power((90 - fck) / 100, 4);
    qlim := 0.8 * bduct - 0.45;
  end;

  // Unit conversion: transform to kN and cm
  amk := 100 * mk;  // kN.m to kN.cm
  fck := fck / 10;  // MPa to kN/cm²
  fyk := fyk / 10;  // MPa to kN/cm²
  es := 100 * es;   // GPa to kN/cm²

  // Design resistances
  fcd := fck / gamac;
  tcd := alfac * fcd;
  fyd := fyk / gamas;
  amd := gamaf * amk;

  // Geometric parameter
  delta := dl / d;

  // Limit moment
  amilim := alamb * qlim * (1 - 0.5 * alamb * qlim);

  // Reduced applied moment
  ami := amd / (bw * d * d * tcd);

  // Check if single or double reinforcement is needed
  if ami <= amilim then
  begin
    // Single reinforcement
    qsi := (1 - sqrt(1 - 2 * ami)) / alamb;
    As_trac := alamb * qsi * bw * d * tcd / fyd;
    As_comp := 0;
    verificacao := 'OK';

    // Determine domain based on qsi
    if qsi <= (eu / (eu + 10)) then
      dominio := 2
    else if qsi <= qlim then
      dominio := 3
    else
      dominio := 4;
  end
  else
  begin
    // Double reinforcement

    // Avoid double reinforcement in domain 2
    qsia := eu / (eu + 10);
    if qlim < qsia then
    begin
      verificacao := 'ERRO: Armadura dupla no dominio 2. Aumente as dimensoes da secao';
      As_trac := 0;
      As_comp := 0;
      dominio := 0;
      Exit;
    end;

    // Eliminate case where qlim < delta (compression steel would be in tension)
    if qlim <= delta then
    begin
      verificacao := 'ERRO: Aumente as dimensoes da secao transversal';
      As_trac := 0;
      As_comp := 0;
      dominio := 0;
      Exit;
    end;

    // Strain in compression reinforcement
    esl := eu * (qlim - delta) / qlim;
    esl := esl / 1000;  // Convert to proper units

    // Stress in compression reinforcement
    tsl := CalcularTensaoAco(es, esl, fyd);

    As_comp := (ami - amilim) * bw * d * tcd / ((1 - delta) * tsl);
    As_trac := (alamb * qlim + (ami - amilim) / (1 - delta)) * bw * d * tcd / fyd;

    dominio := 3;  // Double reinforcement typically in domain 3
    verificacao := 'OK - Armadura dupla';
  end;

  // Minimum reinforcement
  a := 2.0 / 3.0;
  fck := 10 * fck;  // Convert back to MPa
  fyd_orig := 10 * fyd;  // Convert to MPa

  if fck <= 50 then
    romin := 0.078 * Power(fck, a) / fyd_orig
  else
    romin := 0.5512 * Ln(1 + 0.11 * fck) / fyd_orig;

  if romin < 0.0015 then
    romin := 0.0015;

  asmin := romin * bw * h;

  if As_trac < asmin then
  begin
    As_trac := asmin;
    verificacao := 'OK - Armadura minima';
  end;

  // Calculate reinforcement ratio
  taxa := (As_trac / (bw * h)) * 100;  // Percentage
end;

procedure OutputResults;
var
  verif_suffix: string;
begin
  // Field-specific output (highest priority)
  if fieldOutput <> '' then
  begin
    if (fieldOutput = 'As') or (fieldOutput = 'as') then
      WriteLn(As_trac:0:2)
    else if (fieldOutput = 'As_comp') or (fieldOutput = 'as_comp') or (fieldOutput = 'Asl') or (fieldOutput = 'As''') then
      WriteLn(As_comp:0:2)
    else if fieldOutput = 'dominio' then
      WriteLn(dominio)
    else if fieldOutput = 'taxa' then
      WriteLn(taxa:0:4)
    else if fieldOutput = 'ami' then
      WriteLn(ami:0:6)
    else if fieldOutput = 'amilim' then
      WriteLn(amilim:0:6)
    else if fieldOutput = 'verificacao' then
      WriteLn(verificacao)
    else
    begin
      WriteLn(StdErr, 'Error: Unknown field "', fieldOutput, '"');
      WriteLn(StdErr, 'Valid fields: As, As_comp, dominio, taxa, ami, amilim, verificacao');
      Halt(2);
    end;
    Exit;
  end;

  // JSON output
  if jsonOutput then
  begin
    WriteLn('{');
    WriteLn('  "As": ', As_trac:0:2, ',');
    WriteLn('  "As_comp": ', As_comp:0:2, ',');
    WriteLn('  "dominio": ', dominio, ',');
    WriteLn('  "taxa": ', taxa:0:4, ',');
    WriteLn('  "ami": ', ami:0:6, ',');
    WriteLn('  "amilim": ', amilim:0:6, ',');
    WriteLn('  "verificacao": "', verificacao, '"');
    WriteLn('}');
    Exit;
  end;

  // Verbose output (human-readable)
  if verboseOutput then
  begin
    WriteLn('============================================');
    WriteLn('  FLEXAO NORMAL SIMPLES - NBR 6118');
    WriteLn('============================================');
    WriteLn('');
    WriteLn('Armadura tracionada (As):  ', As_trac:8:2, ' cm2');
    WriteLn('Armadura comprimida (Asl): ', As_comp:8:2, ' cm2');
    WriteLn('Dominio:                   ', dominio);
    WriteLn('Taxa de armadura:          ', taxa:8:4, ' %');
    WriteLn('Momento reduzido (μ):      ', ami:8:6);
    WriteLn('Momento limite (μlim):     ', amilim:8:6);
    WriteLn('');
    WriteLn('Verificacao: ', verificacao);
    WriteLn('============================================');
    Exit;
  end;

  // Default: Concise output for AI agents
  Write('As=', As_trac:0:2);
  Write(' As''=', As_comp:0:2);
  Write(' dominio=', dominio);
  Write(' rho=', taxa:0:2);

  // Only add verification suffix if it's not plain "OK"
  if verificacao <> 'OK' then
  begin
    verif_suffix := StringReplace(verificacao, ' ', '_', [rfReplaceAll]);
    verif_suffix := StringReplace(verif_suffix, '-', '', [rfReplaceAll]);
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

    // Exit code based on verification
    if Pos('ERRO', verificacao) > 0 then
      Halt(1)
    else
      Halt(0);
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'Error: ', E.Message);
      Halt(1);
    end;
  end;
end.
