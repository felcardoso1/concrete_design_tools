program FlexoTracao;

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
  bw, h, d, dl: Double;
  fck, fyk, es: Double;
  mk, nk: Double;  // Moment and axial tension
  gamac, gamas, gamaf, bduct: Double;
  jsonOutput, verboseOutput: Boolean;
  fieldOutput: string;

  // Output variables
  As_total, Asl_total: Double;  // Reinforcement areas
  idom: Integer;                 // Domain (1 or 2)
  verificacao: string;
  warnings: string;               // Accumulated warnings (only shown in verbose/JSON)

// Field alias resolution
function ResolveFieldName(const fieldName: string): string;
begin
  Result := LowerCase(fieldName);
  // Map aliases
  if (Result = 'as') then
    Result := 'As_total'
  else if (Result = 'asl') or (Result = 'as''') then
    Result := 'Asl_total'
  else if (Result = 'dominio') then
    Result := 'idom'
  else
    Result := fieldName;
end;

// Calculate stress in steel
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
  WriteLn(StdErr, 'Usage: flexo_tracao --bw=VALUE --h=VALUE --d=VALUE --mk=VALUE --nk=VALUE --fck=VALUE --fyk=VALUE [OPTIONS]');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Required Parameters:');
  WriteLn(StdErr, '  --bw       Section width (cm)');
  WriteLn(StdErr, '  --h        Section height (cm)');
  WriteLn(StdErr, '  --d        Effective depth (cm)');
  WriteLn(StdErr, '  --mk       Characteristic bending moment (kNm)');
  WriteLn(StdErr, '  --nk       Characteristic tension force (kN) - positive for tension');
  WriteLn(StdErr, '  --fck      Characteristic concrete strength (MPa)');
  WriteLn(StdErr, '  --fyk      Characteristic steel yield strength (MPa)');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Optional Parameters:');
  WriteLn(StdErr, '  --dl       Distance to compression face steel (cm), default: 0.1*d');
  WriteLn(StdErr, '  --es       Steel elastic modulus (GPa), default: 200');
  WriteLn(StdErr, '  --gamac    Concrete safety factor, default: 1.4');
  WriteLn(StdErr, '  --gamas    Steel safety factor, default: 1.15');
  WriteLn(StdErr, '  --gamaf    Load safety factor, default: 1.4');
  WriteLn(StdErr, '  --bduct    Ductility coefficient, default: 1.0');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Output Formats:');
  WriteLn(StdErr, '  (default)  Concise output: As=X.XX Asl=X.XX dominio=N');
  WriteLn(StdErr, '  --verbose  Human-readable formatted output');
  WriteLn(StdErr, '  --json     JSON format');
  WriteLn(StdErr, '  --field=X  Output only specific field');
  WriteLn(StdErr, '             Valid fields: As, Asl, dominio, verificacao');
  WriteLn(StdErr, '             Aliases: As=As_total, Asl=Asl_total');
  WriteLn(StdErr, '             Use --field=list to show all fields and aliases');
  WriteLn(StdErr, '  --version  Show version information');
  WriteLn(StdErr, '  --help-all Show all available tools');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Exit Codes:');
  WriteLn(StdErr, '  0  Success');
  WriteLn(StdErr, '  1  Calculation error (insufficient section)');
  WriteLn(StdErr, '  2  Invalid arguments');
  WriteLn(StdErr, '  4  Invalid parameter range');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Examples:');
  WriteLn(StdErr, '  flexo_tracao --bw=20 --h=50 --d=46 --mk=100 --nk=200 --fck=25 --fyk=500');
  WriteLn(StdErr, '  flexo_tracao --bw=20 --h=50 --d=46 --mk=100 --nk=200 --fck=25 --fyk=500 --field=As');
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
  WriteLn('flexo_tracao v1.0.0');
  WriteLn('NBR 6118:2014 compliant');
  WriteLn('Compiled: 2025-12-30');
  Halt(EXIT_SUCCESS);
end;

procedure ShowFieldList;
begin
  WriteLn('Available fields: As, Asl, dominio, verificacao');
  WriteLn('Aliases: As=As_total, Asl=Asl_total');
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
  
  if h <= 0 then
  begin
    WriteLn(StdErr, 'ERRO: h must be positive (received: ', h:0:1, ' cm)');
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
  
  if mk < 0 then
  begin
    WriteLn(StdErr, 'ERRO: mk must be non-negative (received: ', mk:0:2, ' kN.m)');
    Halt(EXIT_INVALID_RANGE);
  end;
  
  if nk <= 0 then
  begin
    WriteLn(StdErr, 'ERRO: nk must be positive for tension (received: ', nk:0:2, ' kN)');
    WriteLn(StdErr, 'Note: nk > 0 means tension force');
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

  s := GetParamValue('h');
  if s = '' then ShowUsage;
  h := StrToFloat(s);

  s := GetParamValue('d');
  if s = '' then ShowUsage;
  d := StrToFloat(s);

  s := GetParamValue('mk');
  if s = '' then ShowUsage;
  mk := StrToFloat(s);

  s := GetParamValue('nk');
  if s = '' then ShowUsage;
  nk := StrToFloat(s);

  s := GetParamValue('fck');
  if s = '' then ShowUsage;
  fck := StrToFloat(s);

  s := GetParamValue('fyk');
  if s = '' then ShowUsage;
  fyk := StrToFloat(s);

  // Optional parameters
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
  amd, aand: Double;
  delta: Double;
  ani, ami, ami0: Double;
  amisd, amilim: Double;
  qsi, qsia: Double;
  w, wl: Double;
  esl, tsl: Double;
  a, romin, r1min, r2min, asmin, astot: Double;
begin
  // Rectangular stress block parameters
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

  // Unit conversion
  amd := 100 * mk;    // kNm to kNcm
  fck := fck / 10;    // MPa to kN/cm²
  fyk := fyk / 10;
  es := 100 * es;     // GPa to kN/cm²

  // Design resistances
  fcd := fck / gamac;
  tcd := alfac * fcd;
  fyd := fyk / gamas;

  // Design loads
  amd := gamaf * amd;
  aand := gamaf * nk;

  // Geometric parameter
  delta := dl / d;

  // Reduced loads
  ani := aand / (bw * d * tcd);
  ami := amd / (bw * d * d * tcd);

  // Moment defining end of domain 1
  ami0 := 0.5 * (1 - delta) * ani;

  if ami <= ami0 then
  begin
    // Domain 1: Small eccentricity tension
    idom := 1;

    // Mechanical reinforcement ratios
    wl := (ami0 - ami) / (1 - delta);
    w := (ami0 + ami) / (1 - delta);
  end
  else
  begin
    // Domains 2 and 3: Large eccentricity tension
    idom := 2;

    // Equivalent moment
    amisd := ami - ami0;

    // Limit moment
    amilim := alamb * qlim * (1 - 0.5 * alamb * qlim);

    if amisd <= amilim then
    begin
      // Single reinforcement
      qsi := (1 - sqrt(1 - 2 * amisd)) / alamb;
      w := alamb * qsi + ani;
      wl := 0;
    end
    else
    begin
      // Double reinforcement
      qsia := eu / (eu + 10);
      if qlim < qsia then
      begin
        verificacao := 'ERRO: Double reinforcement in domain 2 not allowed' + LineEnding +
                       'CURRENT: bw=' + FloatToStr(bw) + 'cm, h=' + FloatToStr(h) + 'cm, d=' + FloatToStr(d) + 'cm, Mk=' + FloatToStr(mk) + 'kN.m, Nk=' + FloatToStr(nk) + 'kN, fck=' + FloatToStr(fck) + 'MPa' + LineEnding +
                       'SUGGESTION: Increase section dimensions (h or bw) to avoid domain 2 with double reinforcement';
        As_total := 0;
        Asl_total := 0;
        Exit;
      end;

      if qlim <= delta then
      begin
        verificacao := 'ERRO: Section insufficient - compression steel would be in tension' + LineEnding +
                       'CURRENT: bw=' + FloatToStr(bw) + 'cm, h=' + FloatToStr(h) + 'cm, d=' + FloatToStr(d) + 'cm, Mk=' + FloatToStr(mk) + 'kN.m, Nk=' + FloatToStr(nk) + 'kN, fck=' + FloatToStr(fck) + 'MPa' + LineEnding +
                       'SUGGESTION: Increase section dimensions (h or bw)';
        As_total := 0;
        Asl_total := 0;
        Exit;
      end;

      // Strain in compression reinforcement
      esl := eu * (qlim - delta) / qlim;
      esl := esl / 1000;

      // Stress in compression reinforcement
      tsl := CalcularTensaoAco(es, esl, fyd);

      // Mechanical reinforcement ratios
      wl := (amisd - amilim) * fyd / ((1 - delta) * tsl);
      w := alamb * qlim + (amisd - amilim) / (1 - delta) + ani;
    end;
  end;

  // Reinforcement areas
  As_total := w * bw * d * tcd / fyd;
  Asl_total := wl * bw * d * tcd / fyd;

  // Minimum reinforcement
  // Convert back to MPa
  fck := 10 * fck;
  fyd := 10 * fyd;

  // Minimum for pure tension
  a := 2.0 / 3.0;
  if fck <= 50 then
    r1min := 0.39 * Power(fck, a) / fyd
  else
    r1min := 2.756 * Ln(1 + 0.11 * fck) / fyd;

  // Minimum for pure bending
  if fck <= 50 then
    r2min := 0.078 * Power(fck, a) / fyd
  else
    r2min := 0.5512 * Ln(1 + 0.11 * fck) / fyd;

  if r2min < 0.0015 then
    r2min := 0.0015;

  // Interpolate minimum based on domain
  if idom = 2 then
    romin := r2min
  else
    romin := r2min + (r1min - r2min) * (ami0 - ami) / ami0;

  asmin := romin * bw * h;

  // Apply minimum reinforcement
  if (idom = 2) and (As_total < asmin) then
    As_total := asmin;

  if idom = 1 then
  begin
    astot := As_total + Asl_total;
    if astot < asmin then
    begin
      As_total := As_total * asmin / astot;
      Asl_total := Asl_total * asmin / astot;
    end;
  end;

  verificacao := 'OK';
end;

procedure OutputResults;
var
  verif_suffix: string;
  resolvedField: string;
begin
  // Field-specific output
  if fieldOutput <> '' then
  begin
    resolvedField := ResolveFieldName(fieldOutput);
    
    if (resolvedField = 'As_total') or (LowerCase(resolvedField) = 'as') then
      WriteLn(As_total:0:2)
    else if (resolvedField = 'Asl_total') or (LowerCase(resolvedField) = 'asl') then
      WriteLn(Asl_total:0:2)
    else if (resolvedField = 'idom') or (LowerCase(resolvedField) = 'dominio') then
      WriteLn(idom)
    else if (LowerCase(resolvedField) = 'verificacao') then
      WriteLn(verificacao)
    else
    begin
      WriteLn(StdErr, 'Error: Unknown field "', fieldOutput, '"');
      WriteLn(StdErr, 'Valid fields: As, Asl, dominio, verificacao');
      WriteLn(StdErr, 'Aliases: As=As_total, Asl=Asl_total');
      Halt(EXIT_INVALID_ARGS);
    end;
    Exit;
  end;

  // JSON output
  if jsonOutput then
  begin
    WriteLn('{');
    WriteLn('  "As": ', As_total:0:2, ',');
    WriteLn('  "Asl": ', Asl_total:0:2, ',');
    WriteLn('  "dominio": ', idom, ',');
    WriteLn('  "verificacao": "', verificacao, '",');
    if warnings <> '' then
      WriteLn('  "warnings": "', StringReplace(warnings, LineEnding, ' ', [rfReplaceAll]), '"')
    else
      WriteLn('  "warnings": null');
    WriteLn('}');
    Exit;
  end;

  // Verbose output
  if verboseOutput then
  begin
    WriteLn('============================================');
    WriteLn('  FLEXO-TRACAO - NBR 6118');
    WriteLn('============================================');
    WriteLn('');
    WriteLn('Armadura principal (As):   ', As_total:8:2, ' cm2');
    WriteLn('Armadura secundaria (Asl): ', Asl_total:8:2, ' cm2');
    WriteLn('Dominio:                   ', idom);
    if idom = 1 then
      WriteLn('  (Pequena excentricidade)')
    else
      WriteLn('  (Grande excentricidade)');
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

  // Default: Concise output
  Write('As=', As_total:0:2);
  Write(' Asl=', Asl_total:0:2);
  Write(' dominio=', idom);

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
