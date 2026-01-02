program Torcao;

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
  bw, h, dl: Double;
  fck, fyk, es: Double;
  mk, vk, tk: Double;  // Moment, shear, torsion
  gamac, gamas, gamaf, bduct: Double;
  jsonOutput, verboseOutput: Boolean;
  fieldOutput: string;

  // Output variables
  As_flexao, Asl_flexao: Double;  // Flexural reinforcement
  Aswv: Double;                    // Stirrups for shear (cm²/m)
  Aswt: Double;                    // Stirrups for torsion (cm²/m)
  Asw_total: Double;               // Total stirrups (cm²/m)
  Aslt: Double;                    // Longitudinal reinforcement for torsion (cm²)
  smax: Double;                    // Maximum spacing (cm)
  verificacao: string;
  warnings: string;                 // Accumulated warnings (only shown in verbose/JSON)

// Field alias resolution
function ResolveFieldName(const fieldName: string): string;
begin
  Result := LowerCase(fieldName);
  // Map aliases
  if (Result = 'asw') then
    Result := 'Asw'
  else if (Result = 'as') then
    Result := 'As_flexao'
  else if (Result = 'asl') or (Result = 'as''') then
    Result := 'Asl_flexao'
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
  WriteLn(StdErr, 'Usage: torcao --bw=VALUE --h=VALUE --dl=VALUE --fck=VALUE --fyk=VALUE --mk=VALUE --vk=VALUE --tk=VALUE [OPTIONS]');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Required Parameters:');
  WriteLn(StdErr, '  --bw       Section width (cm)');
  WriteLn(StdErr, '  --h        Section height (cm)');
  WriteLn(StdErr, '  --dl       Cover (cm)');
  WriteLn(StdErr, '  --fck      Characteristic concrete strength (MPa)');
  WriteLn(StdErr, '  --fyk      Characteristic steel yield strength (MPa)');
  WriteLn(StdErr, '  --mk       Characteristic bending moment (kN.m)');
  WriteLn(StdErr, '  --vk       Characteristic shear force (kN)');
  WriteLn(StdErr, '  --tk       Characteristic torsion moment (kN.m)');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Optional Parameters:');
  WriteLn(StdErr, '  --es       Steel elastic modulus (GPa), default: 200');
  WriteLn(StdErr, '  --gamac    Concrete safety factor, default: 1.4');
  WriteLn(StdErr, '  --gamas    Steel safety factor, default: 1.15');
  WriteLn(StdErr, '  --gamaf    Load safety factor, default: 1.4');
  WriteLn(StdErr, '  --bduct    Ductility coefficient, default: 1.0');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Output Formats:');
  WriteLn(StdErr, '  (default)  Concise output: Asw=X.XX Aslt=X.XX smax=X.X');
  WriteLn(StdErr, '  --verbose  Human-readable formatted output');
  WriteLn(StdErr, '  --json     JSON format');
  WriteLn(StdErr, '  --field=X  Output only specific field');
  WriteLn(StdErr, '             Valid fields: Asw, Aswv, Aswt, Aslt, smax, As, Asl, verificacao');
  WriteLn(StdErr, '             Aliases: As=As_flexao, Asl=Asl_flexao');
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
  WriteLn(StdErr, '  torcao --bw=20 --h=50 --dl=3 --fck=25 --fyk=500 --mk=100 --vk=50 --tk=20');
  WriteLn(StdErr, '  torcao --bw=20 --h=50 --dl=3 --fck=25 --fyk=500 --mk=100 --vk=50 --tk=20 --field=Asw');
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
  WriteLn('torcao v1.0.0');
  WriteLn('NBR 6118:2014 compliant');
  WriteLn('Compiled: 2025-12-30');
  Halt(EXIT_SUCCESS);
end;

procedure ShowFieldList;
begin
  WriteLn('Available fields: Asw, Aswv, Aswt, Aslt, smax, As, Asl, verificacao');
  WriteLn('Aliases: As=As_flexao, Asl=Asl_flexao');
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
  
  if dl <= 0 then
  begin
    WriteLn(StdErr, 'ERRO: dl must be positive (received: ', dl:0:1, ' cm)');
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
  
  if vk < 0 then
  begin
    WriteLn(StdErr, 'ERRO: vk must be non-negative (received: ', vk:0:2, ' kN)');
    Halt(EXIT_INVALID_RANGE);
  end;
  
  if tk < 0 then
  begin
    WriteLn(StdErr, 'ERRO: tk must be non-negative (received: ', tk:0:2, ' kN.m)');
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

  s := GetParamValue('dl');
  if s = '' then ShowUsage;
  dl := StrToFloat(s);

  s := GetParamValue('fck');
  if s = '' then ShowUsage;
  fck := StrToFloat(s);

  s := GetParamValue('fyk');
  if s = '' then ShowUsage;
  fyk := StrToFloat(s);

  s := GetParamValue('mk');
  if s = '' then ShowUsage;
  mk := StrToFloat(s);

  s := GetParamValue('vk');
  if s = '' then ShowUsage;
  vk := StrToFloat(s);

  s := GetParamValue('tk');
  if s = '' then ShowUsage;
  tk := StrToFloat(s);

  // Optional parameters
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
  amd, vd, td: Double;
  d, delta: Double;
  ami, amilim, qsi, qsia: Double;
  esl, tsl: Double;
  a, romin, asmin: Double;
  // Shear variables
  twd, twu, av, tc, tald: Double;
  aswmin: Double;
  // Torsion variables
  c1, t0, t, ae, up, tmax: Double;
  ttd, ttu, soma: Double;
  fctm, fykmax, rowmin, aslmin: Double;
begin
  // === FLEXURAL DESIGN (from flexao.pas) ===

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
  amd := 100 * mk;      // kN.m to kN.cm
  td := 100 * tk;       // kN.m to kN.cm
  es := 100 * es;       // GPa to kN/cm²

  // Design resistances (kN/cm²)
  fcd := fck / (10 * gamac);
  tcd := alfac * fcd;
  fyd := fyk / (10 * gamas);

  // Design loads
  amd := gamaf * amd;
  vd := gamaf * vk;
  td := gamaf * td;

  // Effective depth
  d := h - dl;
  delta := dl / d;

  // Flexural design
  amilim := alamb * qlim * (1 - 0.5 * alamb * qlim);
  ami := amd / (bw * d * d * tcd);

  if ami <= amilim then
  begin
    // Single reinforcement
    qsi := (1 - sqrt(1 - 2 * ami)) / alamb;
    As_flexao := alamb * qsi * bw * d * tcd / fyd;
    Asl_flexao := 0;
  end
  else
  begin
    // Double reinforcement
    qsia := eu / (eu + 10);
    if qlim < qsia then
    begin
      verificacao := 'ERRO: Double reinforcement in domain 2 not allowed' + LineEnding +
                     'CURRENT: bw=' + FloatToStr(bw) + 'cm, h=' + FloatToStr(h) + 'cm, Mk=' + FloatToStr(mk) + 'kN.m, fck=' + FloatToStr(fck) + 'MPa' + LineEnding +
                     'SUGGESTION: Increase section dimensions (h or bw) to avoid domain 2 with double reinforcement';
      Exit;
    end;

    if qlim <= delta then
    begin
      verificacao := 'ERRO: Section insufficient - compression steel would be in tension' + LineEnding +
                     'CURRENT: bw=' + FloatToStr(bw) + 'cm, h=' + FloatToStr(h) + 'cm, Mk=' + FloatToStr(mk) + 'kN.m, fck=' + FloatToStr(fck) + 'MPa' + LineEnding +
                     'SUGGESTION: Increase section dimensions (h or bw)';
      Exit;
    end;

    esl := eu * (qlim - delta) / qlim;
    esl := esl / 1000;
    tsl := CalcularTensaoAco(es, esl, fyd);
    Asl_flexao := (ami - amilim) * bw * d * tcd / ((1 - delta) * tsl);
    As_flexao := (alamb * qlim + (ami - amilim) / (1 - delta)) * bw * d * tcd / fyd;
  end;

  // Minimum flexural reinforcement
  a := 2.0 / 3.0;
  fyd := 10 * fyd;  // Convert to MPa for minimum reinforcement calc
  if fck <= 50 then
    romin := 0.078 * Power(fck, a) / fyd
  else
    romin := 0.5512 * Ln(1 + 0.11 * fck) / fyd;

  if romin < 0.0015 then
    romin := 0.0015;

  asmin := romin * bw * h;
  if As_flexao < asmin then
    As_flexao := asmin;

  // === SHEAR AND TORSION DESIGN ===

  // Convert fcd to MPa for shear/torsion
  fcd := 10 * fcd;

  // Shear stress
  twd := vd / (bw * d);
  twd := 10 * twd;  // Convert to MPa

  // Ultimate shear stress
  av := 1 - fck / 250;
  twu := 0.27 * av * fcd;

  // Torsion parameters - equivalent hollow section
  c1 := dl;
  t0 := bw * h / (2 * (bw + h));

  if t0 >= 2 * c1 then
  begin
    // Case 1
    t := t0;
    ae := (bw - t) * (h - t);
    up := 2 * (bw + h - 2 * t);
  end
  else
  begin
    // Case 2
    t := t0;
    tmax := bw - 2 * c1;
    if t > tmax then
      t := tmax;
    ae := (bw - 2 * c1) * (h - 2 * c1);
    up := 2 * (bw + h - 4 * c1);
  end;

  // Torsional shear stress
  ttd := td / (2 * ae * t);
  ttd := 10 * ttd;  // Convert to MPa

  // Ultimate torsional stress
  ttu := 0.25 * av * fcd;

  // Combined verification
  soma := ttd / ttu + twd / twu;
  if soma > 1 then
  begin
    verificacao := 'ERRO: Compression strut crushing (combined shear + torsion)' + LineEnding +
                   'CURRENT: bw=' + FloatToStr(bw) + 'cm, h=' + FloatToStr(h) + 'cm, Vk=' + FloatToStr(vk) + 'kN, Tk=' + FloatToStr(tk) + 'kN.m, fck=' + FloatToStr(fck) + 'MPa' + LineEnding +
                   'SUGGESTION: Increase section dimensions (bw or h) OR reduce loads';
    Exit;
  end;

  // Maximum spacing
  if soma <= 0.67 then
  begin
    smax := 0.6 * d;
    if smax > 30 then
      smax := 30;
  end
  else
  begin
    smax := 0.3 * d;
    if smax > 20 then
      smax := 20;
  end;

  // Shear reinforcement
  if fck <= 50 then
  begin
    a := 2.0 / 3.0;
    tc := 0.126 * Power(fck, a) / gamac;
  end
  else
    tc := 0.8904 * Ln(1 + 0.11 * fck) / gamac;

  tald := 1.11 * (twd - tc);
  if tald < 0 then
    tald := 0;

  fyd := fyd / 10;  // Convert back to kN/cm²
  if fyd > 435 then
    fyd := 435;

  Aswv := 100 * bw * tald / fyd;

  // Torsion reinforcement
  fyd := fyd / 10;  // Back to kN/cm²
  Aswt := 100 * td / (2 * ae * fyd);
  Aslt := td * up / (2 * ae * fyd);

  // Minimum reinforcement
  fykmax := fyk;
  if fykmax > 500 then
    fykmax := 500;

  if fck <= 50 then
  begin
    a := 2.0 / 3.0;
    fctm := 0.3 * Power(fck, a);
  end
  else
    fctm := 2.12 * Ln(1 + 0.11 * fck);

  rowmin := 0.2 * fctm / fykmax;

  // Total stirrups
  Asw_total := Aswv + 2 * Aswt;
  aswmin := rowmin * 100 * bw;
  if Asw_total < aswmin then
    Asw_total := aswmin;

  // Minimum longitudinal torsion reinforcement
  aslmin := 0.5 * rowmin * up * bw;
  if Aslt < aslmin then
    Aslt := aslmin;

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
    
    if (resolvedField = 'Asw') or (LowerCase(resolvedField) = 'asw') then
      WriteLn(Asw_total:0:2)
    else if (LowerCase(resolvedField) = 'aswv') then
      WriteLn(Aswv:0:2)
    else if (LowerCase(resolvedField) = 'aswt') then
      WriteLn(Aswt:0:2)
    else if (LowerCase(resolvedField) = 'aslt') then
      WriteLn(Aslt:0:2)
    else if (LowerCase(resolvedField) = 'smax') then
      WriteLn(smax:0:1)
    else if (resolvedField = 'As_flexao') or (LowerCase(resolvedField) = 'as') then
      WriteLn(As_flexao:0:2)
    else if (resolvedField = 'Asl_flexao') or (LowerCase(resolvedField) = 'asl') then
      WriteLn(Asl_flexao:0:2)
    else if (LowerCase(resolvedField) = 'verificacao') then
      WriteLn(verificacao)
    else
    begin
      WriteLn(StdErr, 'Error: Unknown field "', fieldOutput, '"');
      WriteLn(StdErr, 'Valid fields: Asw, Aswv, Aswt, Aslt, smax, As, Asl, verificacao');
      WriteLn(StdErr, 'Aliases: As=As_flexao, Asl=Asl_flexao');
      Halt(EXIT_INVALID_ARGS);
    end;
    Exit;
  end;

  // JSON output
  if jsonOutput then
  begin
    WriteLn('{');
    WriteLn('  "Asw_total": ', Asw_total:0:2, ',');
    WriteLn('  "Aswv": ', Aswv:0:2, ',');
    WriteLn('  "Aswt": ', Aswt:0:2, ',');
    WriteLn('  "Aslt": ', Aslt:0:2, ',');
    WriteLn('  "smax": ', smax:0:1, ',');
    WriteLn('  "As_flexao": ', As_flexao:0:2, ',');
    WriteLn('  "Asl_flexao": ', Asl_flexao:0:2, ',');
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
    WriteLn('  TORCAO - NBR 6118');
    WriteLn('============================================');
    WriteLn('');
    WriteLn('ESTRIBOS (dois ramos):');
    WriteLn('  Para cortante:          ', Aswv:8:2, ' cm2/m');
    WriteLn('  Para torcao:            ', Aswt:8:2, ' cm2/m');
    WriteLn('  Total:                  ', Asw_total:8:2, ' cm2/m');
    WriteLn('  Espacamento maximo:     ', smax:8:1, ' cm');
    WriteLn('');
    WriteLn('ARMADURA LONGITUDINAL:');
    WriteLn('  Para flexao (As):       ', As_flexao:8:2, ' cm2');
    WriteLn('  Para flexao (Asl):      ', Asl_flexao:8:2, ' cm2');
    WriteLn('  Para torcao (Aslt):     ', Aslt:8:2, ' cm2');
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
  Write('Asw=', Asw_total:0:2);
  Write(' Aslt=', Aslt:0:2);
  Write(' smax=', smax:0:1);

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
