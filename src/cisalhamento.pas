program Cisalhamento;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

var
  // Input variables
  bw, d: Double;
  fck, fyk: Double;
  vk: Double;
  gamac, gamas, gamaf: Double;
  jsonOutput, verboseOutput: Boolean;
  fieldOutput: string;

  // Output variables
  asw: Double;              // Stirrup area per meter (cm²/m)
  twd, twu: Double;         // Shear stresses
  tc, td: Double;           // Concrete contribution and design stress
  aswmin: Double;           // Minimum stirrup area
  verificacao: string;      // Status message

procedure ShowUsage;
begin
  WriteLn(StdErr, 'Usage: cisalhamento --bw=VALUE --d=VALUE --fck=VALUE --fyk=VALUE --vk=VALUE [OPTIONS]');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Required Parameters:');
  WriteLn(StdErr, '  --bw       Section width (cm)');
  WriteLn(StdErr, '  --d        Effective depth (cm)');
  WriteLn(StdErr, '  --fck      Characteristic concrete strength (MPa)');
  WriteLn(StdErr, '  --fyk      Characteristic steel yield strength (MPa)');
  WriteLn(StdErr, '  --vk       Characteristic shear force (kN)');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Optional Parameters:');
  WriteLn(StdErr, '  --gamac    Concrete safety factor, default: 1.4');
  WriteLn(StdErr, '  --gamas    Steel safety factor, default: 1.15');
  WriteLn(StdErr, '  --gamaf    Load safety factor, default: 1.4');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Output Formats:');
  WriteLn(StdErr, '  (default)  Concise output for AI agents: Asw=X.XX twd=X.XX twu=X.XX');
  WriteLn(StdErr, '  --verbose  Human-readable formatted output');
  WriteLn(StdErr, '  --json     JSON format for structured parsing');
  WriteLn(StdErr, '  --field=X  Output only specific field');
  WriteLn(StdErr, '             Valid fields: Asw, twd, twu, tc, aswmin, verificacao');
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

  s := GetParamValue('fck');
  if s = '' then ShowUsage;
  fck := StrToFloat(s);

  s := GetParamValue('fyk');
  if s = '' then ShowUsage;
  fyk := StrToFloat(s);

  s := GetParamValue('vk');
  if s = '' then ShowUsage;
  vk := StrToFloat(s);

  // Optional parameters with defaults
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
end;

procedure Calculate;
var
  fcd, fyd, vd: Double;
  av, a: Double;
  fctm, fykmax, romin: Double;
begin
  // Design resistances
  fcd := fck / gamac;
  fyd := fyk / gamas;
  vd := gamaf * vk;

  // Conventional shear stress
  twd := vd / (bw * d);
  // Convert to MPa
  twd := 10 * twd;

  // Ultimate shear stress
  av := 1 - fck / 250;
  twu := 0.27 * av * fcd;

  // Check for crushing of compression strut
  if twd > twu then
  begin
    verificacao := 'ERRO: Esmagamento da biela de compressao';
    asw := 0;
    tc := 0;
    td := 0;
    aswmin := 0;
    Exit;
  end;

  // Concrete contribution
  if fck <= 50 then
  begin
    a := 2.0 / 3.0;
    tc := 0.126 * Power(fck, a) / gamac;
  end
  else
    tc := 0.8904 * Ln(1 + 0.11 * fck) / gamac;

  // Design stress for stirrup calculation
  td := 1.11 * (twd - tc);
  if td < 0 then
    td := 0;

  // Limit steel yield stress (NBR-6118)
  if fyd > 435 then
    fyd := 435;

  // Calculate stirrup area
  asw := 100 * bw * td / fyd;

  // Minimum reinforcement
  fykmax := fyk;
  if fykmax > 500 then
    fykmax := 500;

  // Mean tensile strength of concrete
  if fck <= 50 then
  begin
    a := 2.0 / 3.0;
    fctm := 0.3 * Power(fck, a);
  end
  else
    fctm := 2.12 * Ln(1 + 0.11 * fck);

  // Minimum reinforcement ratio
  romin := 0.2 * fctm / fykmax;
  aswmin := romin * 100 * bw;

  // Verification
  if asw < aswmin then
  begin
    asw := aswmin;
    verificacao := 'OK - Armadura minima';
  end
  else
    verificacao := 'OK';
end;

procedure OutputResults;
var
  verif_suffix: string;
begin
  // Field-specific output (highest priority)
  if fieldOutput <> '' then
  begin
    if (fieldOutput = 'Asw') or (fieldOutput = 'asw') then
      WriteLn(asw:0:2)
    else if (fieldOutput = 'twd') then
      WriteLn(twd:0:4)
    else if (fieldOutput = 'twu') then
      WriteLn(twu:0:4)
    else if (fieldOutput = 'tc') then
      WriteLn(tc:0:4)
    else if (fieldOutput = 'aswmin') then
      WriteLn(aswmin:0:2)
    else if (fieldOutput = 'verificacao') then
      WriteLn(verificacao)
    else
    begin
      WriteLn(StdErr, 'Error: Unknown field "', fieldOutput, '"');
      WriteLn(StdErr, 'Valid fields: Asw, twd, twu, tc, aswmin, verificacao');
      Halt(2);
    end;
    Exit;
  end;

  // JSON output
  if jsonOutput then
  begin
    WriteLn('{');
    WriteLn('  "Asw": ', asw:0:2, ',');
    WriteLn('  "twd": ', twd:0:4, ',');
    WriteLn('  "twu": ', twu:0:4, ',');
    WriteLn('  "tc": ', tc:0:4, ',');
    WriteLn('  "aswmin": ', aswmin:0:2, ',');
    WriteLn('  "verificacao": "', verificacao, '"');
    WriteLn('}');
    Exit;
  end;

  // Verbose output (human-readable)
  if verboseOutput then
  begin
    WriteLn('============================================');
    WriteLn('  CISALHAMENTO - NBR 6118');
    WriteLn('============================================');
    WriteLn('');
    WriteLn('Armadura de cisalhamento (Asw): ', asw:8:2, ' cm2/m');
    WriteLn('Tensao de cisalhamento (twd):   ', twd:8:4, ' MPa');
    WriteLn('Tensao ultima (twu):            ', twu:8:4, ' MPa');
    WriteLn('Contribuicao do concreto (tc):  ', tc:8:4, ' MPa');
    WriteLn('Armadura minima (Asw,min):      ', aswmin:8:2, ' cm2/m');
    WriteLn('');
    WriteLn('Verificacao: ', verificacao);
    WriteLn('============================================');
    Exit;
  end;

  // Default: Concise output for AI agents
  Write('Asw=', asw:0:2);
  Write(' twd=', twd:0:4);
  Write(' twu=', twu:0:4);

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
