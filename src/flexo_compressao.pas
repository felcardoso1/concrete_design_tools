program FlexoCompressao;

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
  nk, mk: Double;  // Normal force (compression) and moment
  gamac, gamas, gamaf: Double;
  jsonOutput, verboseOutput: Boolean;
  fieldOutput: string;

  // Output variables
  As_calc: Double;      // Calculated steel area
  As_min: Double;       // Minimum steel area
  As_adopt: Double;     // Steel area to adopt
  x: Double;            // Neutral axis depth
  dominio: Integer;     // Strain domain
  verificacao: string;
  warnings: string;      // Accumulated warnings (only shown in verbose/JSON)

  // Intermediate variables for calculation
  alamb, alfac, eu, e0, akapa: Double;
  fcd, tcd, fyd: Double;
  delta, ani, ami: Double;
  qlim: Double;

// Field alias resolution
function ResolveFieldName(const fieldName: string): string;
begin
  Result := LowerCase(fieldName);
  // Map aliases
  if (Result = 'as') then
    Result := 'As_adopt'
  else if (Result = 'asmin') or (Result = 'as_min') then
    Result := 'As_min'
  else if (Result = 'asadopt') or (Result = 'as_adopt') then
    Result := 'As_adopt'
  else
    Result := fieldName;
end;

// Calculate stress in steel
function CalcularTensaoAco(es_val, esl, fyd_val: Double): Double;
var
  ess, eyd, tsl: Double;
begin
  ess := Abs(esl);
  eyd := fyd_val / es_val;
  if ess < eyd then
    tsl := es_val * ess
  else
    tsl := fyd_val;
  if esl < 0 then
    tsl := -tsl;
  Result := tsl;
end;

procedure ShowUsage;
begin
  WriteLn(StdErr, 'Usage: flexo_compressao --bw=VALUE --h=VALUE --dl=VALUE --nk=VALUE --mk=VALUE --fck=VALUE --fyk=VALUE [OPTIONS]');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Required Parameters:');
  WriteLn(StdErr, '  --bw       Section width (cm)');
  WriteLn(StdErr, '  --h        Section height (cm)');
  WriteLn(StdErr, '  --dl       Distance to reinforcement layer (cm) - cover distance');
  WriteLn(StdErr, '  --nk       Characteristic compression force (kN) - positive for compression');
  WriteLn(StdErr, '  --mk       Characteristic bending moment (kNm)');
  WriteLn(StdErr, '  --fck      Characteristic concrete strength (MPa)');
  WriteLn(StdErr, '  --fyk      Characteristic steel yield strength (MPa)');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Optional Parameters:');
  WriteLn(StdErr, '  --es       Steel elastic modulus (GPa), default: 200');
  WriteLn(StdErr, '  --gamac    Concrete safety factor, default: 1.4');
  WriteLn(StdErr, '  --gamas    Steel safety factor, default: 1.15');
  WriteLn(StdErr, '  --gamaf    Load safety factor, default: 1.4');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Output Formats:');
  WriteLn(StdErr, '  (default)  Concise output: As=X.XX Asmin=X.XX dominio=N');
  WriteLn(StdErr, '  --verbose  Human-readable formatted output');
  WriteLn(StdErr, '  --json     JSON format');
  WriteLn(StdErr, '  --field=X  Output only specific field');
  WriteLn(StdErr, '             Valid fields: As, Asmin, Asadopt, dominio, x, verificacao');
  WriteLn(StdErr, '             Aliases: As=As_adopt, Asmin=As_min');
  WriteLn(StdErr, '             Use --field=list to show all fields and aliases');
  WriteLn(StdErr, '  --version  Show version information');
  WriteLn(StdErr, '  --help-all Show all available tools');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Exit Codes:');
  WriteLn(StdErr, '  0  Success');
  WriteLn(StdErr, '  1  Calculation error');
  WriteLn(StdErr, '  2  Invalid arguments');
  WriteLn(StdErr, '  4  Invalid parameter range');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Examples:');
  WriteLn(StdErr, '  flexo_compressao --bw=20 --h=50 --dl=3 --nk=500 --mk=100 --fck=25 --fyk=500');
  WriteLn(StdErr, '  flexo_compressao --bw=20 --h=50 --dl=3 --nk=500 --mk=100 --fck=25 --fyk=500 --field=As');
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
  WriteLn('flexo_compressao v1.0.0');
  WriteLn('NBR 6118:2014 compliant');
  WriteLn('Compiled: 2025-12-30');
  Halt(EXIT_SUCCESS);
end;

procedure ShowFieldList;
begin
  WriteLn('Available fields: As, Asmin, Asadopt, dominio, x, verificacao');
  WriteLn('Aliases: As=As_adopt, Asmin=As_min');
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
  
  if nk <= 0 then
  begin
    WriteLn(StdErr, 'ERRO: nk must be positive for compression (received: ', nk:0:2, ' kN)');
    WriteLn(StdErr, 'Note: nk > 0 means compression force');
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

  s := GetParamValue('nk');
  if s = '' then ShowUsage;
  nk := StrToFloat(s);

  s := GetParamValue('mk');
  if s = '' then ShowUsage;
  mk := StrToFloat(s);

  s := GetParamValue('fck');
  if s = '' then ShowUsage;
  fck := StrToFloat(s);

  s := GetParamValue('fyk');
  if s = '' then ShowUsage;
  fyk := StrToFloat(s);

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
end;

procedure CalcularFuncao(qsi: Double; var rc, bc, soma1, soma2, f: Double);
var
  c, ql: Double;
  esi1, esi2, tsi1, tsi2: Double;
  beta1, beta2: Double;
begin
  // Two layers: bottom (beta1) and top (beta2)
  beta1 := 1 - delta;  // Bottom layer
  beta2 := delta;      // Top layer

  // Calculate curvature constant c
  ql := eu * beta1 / (eu + 10);

  if qsi <= ql then
    // Domain 2
    c := 0.01 / (beta1 - qsi)
  else if qsi <= 1 then
    // Domains 3, 4, 4a
    c := eu / (1000 * qsi)
  else
    // Domain 5
    c := (e0 / 1000) / (qsi - akapa);

  // Concrete resultant
  if qsi < 1 / alamb then
  begin
    rc := alamb * qsi;
    bc := 0.5 * alamb * qsi;
  end
  else
  begin
    rc := 1;
    bc := 0.5;
  end;

  // Steel layer 1 (bottom)
  esi1 := c * (qsi - beta1);
  tsi1 := CalcularTensaoAco(es, esi1, fyd);

  // Steel layer 2 (top)
  esi2 := c * (qsi - beta2);
  tsi2 := CalcularTensaoAco(es, esi2, fyd);

  // Sums (assuming equal number of bars in each layer for simplicity)
  soma1 := tsi1 + tsi2;
  soma2 := beta1 * tsi1 + beta2 * tsi2;

  // Function value
  f := (ami - 0.5 * ani + rc * bc) * soma1 + (ani - rc) * soma2;
end;

procedure Calculate;
var
  ac: Double;
  aand, amd: Double;
  qi, qf, qk, fi, ff, fk: Double;
  rc, bc, soma1, soma2: Double;
  prod: Double;
  w: Double;
  ani0, romin: Double;
begin
  // Rectangular stress block parameters
  if fck <= 50 then
  begin
    alamb := 0.8;
    alfac := 0.85;
    eu := 3.5;
    e0 := 2.0;
  end
  else
  begin
    alamb := 0.8 - (fck - 50) / 400;
    alfac := 0.85 * (1 - (fck - 50) / 200);
    eu := 2.6 + 35 * Power((90 - fck) / 100, 4);
    e0 := 2.0 + 0.085 * Power(fck - 50, 0.53);
  end;

  // Parameter kapa (domain 5)
  akapa := 1 - e0 / eu;

  // Unit conversion
  fck := fck / 10;    // MPa to kN/cm²
  fyk := fyk / 10;
  es := 100 * es;     // GPa to kN/cm²
  mk := 100 * mk;     // kNm to kNcm

  // Design resistances
  fcd := fck / gamac;
  tcd := alfac * fcd;
  fyd := fyk / gamas;

  // Design loads
  aand := gamaf * nk;
  amd := gamaf * mk;

  // Geometric parameter
  delta := dl / h;

  // Concrete area
  ac := bw * h;

  // Reduced loads
  ani := aand / (ac * tcd);
  ami := amd / (ac * h * tcd);

  // Special case: Pure compression (no moment)
  if ami = 0 then
  begin
    // Pure compression - simplified calculation
    w := (ani - 1) * fyd / fyd;  // Simplified for demonstration
    if w < 0 then
      w := 0;
    As_calc := w * ac * tcd / fyd;
    x := h;
    dominio := 5;
  end
  else
  begin
    // Flexo-compression - bisection method to find neutral axis

    // Initial interval
    qi := 0;
    CalcularFuncao(qi, rc, bc, soma1, soma2, fi);

    qf := 1000;
    CalcularFuncao(qf, rc, bc, soma1, soma2, ff);

    // Expand interval until solution is bracketed
    prod := fi * ff;
    while prod > 0 do
    begin
      qi := qf;
      fi := ff;
      qf := 10 * qf;
      CalcularFuncao(qf, rc, bc, soma1, soma2, ff);
      prod := fi * ff;
    end;

    // Bisection iteration
    fk := 1;
    while Abs(fk) > 0.001 do
    begin
      qk := (qi * ff - qf * fi) / (ff - fi);
      CalcularFuncao(qk, rc, bc, soma1, soma2, fk);
      prod := fk * fi;
      if prod >= 0 then
      begin
        qi := qk;
        fi := fk;
      end
      else
      begin
        qf := qk;
        ff := fk;
      end;
    end;

    // Convergence reached
    x := qk * h;

    // Calculate mechanical reinforcement ratio
    if Abs(soma1) >= Abs(soma2) then
      w := 2 * fyd * (ani - rc) / soma1
    else
      w := 2 * fyd * (0.5 * ani - ami - rc * bc) / soma2;

    if w < 0 then
      w := 0;

    As_calc := w * ac * tcd / fyd;

    // Determine domain
    qlim := eu * (1 - delta) / (eu + 10);
    if qk <= qlim then
      dominio := 2
    else if qk <= 1 then
      dominio := 3
    else
      dominio := 5;
  end;

  // Minimum reinforcement for columns (NBR 6118)
  // Convert back to MPa for minimum calculation
  fck := 10 * fck;
  fyd := 10 * fyd;

  ani0 := aand / (ac * (fck / gamac));
  romin := 0.15 * (fck / gamac) * ani0 / fyd;
  if romin < 0.004 then
    romin := 0.004;

  As_min := romin * ac;

  // Steel area to adopt
  As_adopt := As_calc;
  if As_adopt < As_min then
  begin
    As_adopt := As_min;
    verificacao := 'OK - Armadura minima';
  end
  else
    verificacao := 'OK';

  // Maximum reinforcement check (8% for columns)
  if As_adopt > 0.08 * ac then
  begin
    verificacao := 'AVISO: Armadura > 8% da secao';
  end;
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
    
    if (resolvedField = 'As_adopt') or (LowerCase(resolvedField) = 'as') then
      WriteLn(As_adopt:0:2)
    else if (resolvedField = 'As_min') or (LowerCase(resolvedField) = 'asmin') then
      WriteLn(As_min:0:2)
    else if (LowerCase(resolvedField) = 'asadopt') or (LowerCase(resolvedField) = 'as_adopt') then
      WriteLn(As_adopt:0:2)
    else if (LowerCase(resolvedField) = 'dominio') then
      WriteLn(dominio)
    else if (LowerCase(resolvedField) = 'x') then
      WriteLn(x:0:2)
    else if (LowerCase(resolvedField) = 'verificacao') then
      WriteLn(verificacao)
    else
    begin
      WriteLn(StdErr, 'Error: Unknown field "', fieldOutput, '"');
      WriteLn(StdErr, 'Valid fields: As, Asmin, Asadopt, dominio, x, verificacao');
      WriteLn(StdErr, 'Aliases: As=As_adopt, Asmin=As_min');
      Halt(EXIT_INVALID_ARGS);
    end;
    Exit;
  end;

  // JSON output
  if jsonOutput then
  begin
    WriteLn('{');
    WriteLn('  "As": ', As_calc:0:2, ',');
    WriteLn('  "Asmin": ', As_min:0:2, ',');
    WriteLn('  "Asadopt": ', As_adopt:0:2, ',');
    WriteLn('  "x": ', x:0:2, ',');
    WriteLn('  "dominio": ', dominio, ',');
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
    WriteLn('  FLEXO-COMPRESSAO - NBR 6118');
    WriteLn('============================================');
    WriteLn('');
    WriteLn('Armadura calculada (As):    ', As_calc:8:2, ' cm2');
    WriteLn('Armadura minima (Asmin):    ', As_min:8:2, ' cm2');
    WriteLn('Armadura a adotar (Asadopt):', As_adopt:8:2, ' cm2');
    WriteLn('Linha neutra (x):           ', x:8:2, ' cm');
    WriteLn('Dominio:                    ', dominio);
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
  Write('As=', As_calc:0:2);
  Write(' Asmin=', As_min:0:2);
  Write(' dominio=', dominio);

  if verificacao <> 'OK' then
  begin
    verif_suffix := StringReplace(verificacao, ' ', '_', [rfReplaceAll]);
    verif_suffix := StringReplace(verif_suffix, '-', '', [rfReplaceAll]);
    verif_suffix := StringReplace(verif_suffix, ':', '', [rfReplaceAll]);
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
