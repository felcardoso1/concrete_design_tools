program Cisalhamento;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

const
  EXIT_SUCCESS = 0;
  EXIT_CALC_ERROR = 1;      // Calculation error (insufficient section, etc.)
  EXIT_INVALID_ARGS = 2;     // Invalid arguments/parameters
  EXIT_DOMAIN_4 = 3;         // Domain 4 (brittle failure)
  EXIT_INVALID_RANGE = 4;    // Parameter out of valid range

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
  warnings: string;          // Accumulated warnings (only shown in verbose/JSON)

// Field alias resolution
function ResolveFieldName(const fieldName: string): string;
begin
  Result := LowerCase(fieldName);
  // No aliases for cisalhamento currently, but structure allows for future additions
  if (Result = 'asw') then
    Result := 'Asw'
  else
    Result := fieldName;
end;

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
  WriteLn(StdErr, '             Use --field=list to show all fields and aliases');
  WriteLn(StdErr, '  --version  Show version information');
  WriteLn(StdErr, '  --help-all Show all available tools');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Exit Codes:');
  WriteLn(StdErr, '  0  Success');
  WriteLn(StdErr, '  1  Calculation error (compression strut crushing)');
  WriteLn(StdErr, '  2  Invalid arguments');
  WriteLn(StdErr, '  4  Invalid parameter range');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Examples:');
  WriteLn(StdErr, '  cisalhamento --bw=20 --d=46 --fck=25 --fyk=500 --vk=100');
  WriteLn(StdErr, '  cisalhamento --bw=20 --d=46 --fck=25 --fyk=500 --vk=100 --field=Asw');
  Halt(EXIT_INVALID_ARGS);
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

procedure ShowVersion;
begin
  WriteLn('cisalhamento v1.0.0');
  WriteLn('NBR 6118:2014 compliant');
  WriteLn('Compiled: 2025-12-30');
  Halt(EXIT_SUCCESS);
end;

procedure ShowFieldList;
begin
  WriteLn('Available fields: Asw, twd, twu, tc, aswmin, verificacao');
  WriteLn('Aliases: (none)');
  Halt(EXIT_SUCCESS);
end;

procedure ValidateParameters;
begin
  warnings := '';  // Initialize empty
  
  // Hard errors (exit immediately)
  if fck < 10 then
  begin
    WriteLn(StdErr, 'ERRO: fck must be >= 10 MPa (received: ', fck:0:1, ' MPa)');
    WriteLn(StdErr, 'NBR 6118 valid range: 10-90 MPa');
    Halt(EXIT_INVALID_RANGE);
  end;
  
  if bw < 9 then
  begin
    WriteLn(StdErr, 'ERRO: bw must be >= 9 cm (received: ', bw:0:1, ' cm)');
    Halt(EXIT_INVALID_RANGE);
  end;
  
  if d <= 0 then
  begin
    WriteLn(StdErr, 'ERRO: d must be positive (received: ', d:0:1, ' cm)');
    Halt(EXIT_INVALID_RANGE);
  end;
  
  if fyk <= 0 then
  begin
    WriteLn(StdErr, 'ERRO: fyk must be positive (received: ', fyk:0:1, ' MPa)');
    Halt(EXIT_INVALID_RANGE);
  end;
  
  if vk <= 0 then
  begin
    WriteLn(StdErr, 'ERRO: vk must be positive (received: ', vk:0:2, ' kN)');
    Halt(EXIT_INVALID_RANGE);
  end;
  
  // Soft warnings (accumulate, show only in verbose/JSON)
  if fck > 90 then
    warnings := warnings + 'WARNING: fck=' + FloatToStr(fck) + 
               ' MPa exceeds 90 MPa. Formulas valid up to 90 MPa.' + LineEnding;
end;

procedure ParseArguments;
var
  s: string;
begin
  // Handle special flags that don't need parameters first
  if HasFlag('version') then
    ShowVersion;
  
  fieldOutput := GetParamValue('field');
  if (fieldOutput = 'list') then
    ShowFieldList;
  
  if HasFlag('help') or (ParamCount = 0) then
    ShowUsage;

  jsonOutput := HasFlag('json');
  verboseOutput := HasFlag('verbose');

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
    verificacao := 'ERRO: Compression strut crushing' + LineEnding +
                   'CURRENT: bw=' + FloatToStr(bw) + 'cm, d=' + FloatToStr(d) + 'cm, Vk=' + FloatToStr(vk) + 'kN, fck=' + FloatToStr(fck) + 'MPa' + LineEnding +
                   'SUGGESTION: Increase section dimensions (bw or d) OR reduce shear force';
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
  resolvedField: string;
begin
  // Field-specific output (highest priority)
  if fieldOutput <> '' then
  begin
    resolvedField := ResolveFieldName(fieldOutput);
    
    if (resolvedField = 'Asw') or (LowerCase(resolvedField) = 'asw') then
      WriteLn(asw:0:2)
    else if (LowerCase(resolvedField) = 'twd') then
      WriteLn(twd:0:4)
    else if (LowerCase(resolvedField) = 'twu') then
      WriteLn(twu:0:4)
    else if (LowerCase(resolvedField) = 'tc') then
      WriteLn(tc:0:4)
    else if (LowerCase(resolvedField) = 'aswmin') then
      WriteLn(aswmin:0:2)
    else if (LowerCase(resolvedField) = 'verificacao') then
      WriteLn(verificacao)
    else
    begin
      WriteLn(StdErr, 'Error: Unknown field "', fieldOutput, '"');
      WriteLn(StdErr, 'Valid fields: Asw, twd, twu, tc, aswmin, verificacao');
      Halt(EXIT_INVALID_ARGS);
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
    WriteLn('  "verificacao": "', verificacao, '",');
    if warnings <> '' then
      WriteLn('  "warnings": "', StringReplace(warnings, LineEnding, ' ', [rfReplaceAll]), '"')
    else
      WriteLn('  "warnings": null');
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
    if warnings <> '' then
    begin
      WriteLn('');
      WriteLn('WARNINGS:');
      Write(warnings);
    end;
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
    ValidateParameters;
    Calculate;
    OutputResults;

    // Exit code based on verification
    if Pos('ERRO', verificacao) > 0 then
      Halt(EXIT_CALC_ERROR)
    else
      Halt(EXIT_SUCCESS);
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'Error: ', E.Message);
      Halt(EXIT_CALC_ERROR);
    end;
  end;
end.
