program FlexaoT;

{$mode objfpc}{$H+}

uses
  SysUtils, Math;

// Initialize decimal separator for parsing
procedure InitLocale;
begin
  DefaultFormatSettings.DecimalSeparator := '.';
end;

var
  // Input variables
  bf, hf, bw, h, d, dl: Double;
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
  WriteLn(StdErr, 'Usage: flexaot --bf=VALUE --hf=VALUE --bw=VALUE --h=VALUE --d=VALUE --fck=VALUE --fyk=VALUE --mk=VALUE [OPTIONS]');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'T-Beam Flexural Design - NBR 6118');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Required Parameters:');
  WriteLn(StdErr, '  --bf       Flange width (mesa) (cm)');
  WriteLn(StdErr, '  --hf       Flange thickness (espessura da mesa) (cm)');
  WriteLn(StdErr, '  --bw       Web width (largura da nervura) (cm)');
  WriteLn(StdErr, '  --h        Total section height (cm)');
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
  WriteLn(StdErr, '  (default)  Concise output for AI agents: As=X.XX As''=X.XX dominio=N');
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
  s := GetParamValue('bf');
  if s = '' then ShowUsage;
  bf := StrToFloat(s);

  s := GetParamValue('hf');
  if s = '' then ShowUsage;
  hf := StrToFloat(s);

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
  betaf, betaw: Double;
  rcclim: Double;
  amif, ami0: Double;
  omega, omegal: Double;
  qsia: Double;
  esl, tsl: Double;
  romin, asmin, ac: Double;
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

  // T-beam parameters
  betaf := hf / d;
  betaw := bw / bf;

  // Check if section functions as T-beam
  if alamb * qlim <= betaf then
  begin
    verificacao := 'ERRO: A mesa e muito espessa. Use secao retangular de largura bf';
    As_trac := 0;
    As_comp := 0;
    dominio := 0;
    ami := 0;
    amilim := 0;
    taxa := 0;
    Exit;
  end;

  // Section will be designed as T-beam
  // Reduced applied moment
  ami := amd / (bf * d * d * tcd);

  // Limit moment
  rcclim := betaf + betaw * (alamb * qlim - betaf);
  amilim := betaf * (1 - 0.5 * betaf);
  amilim := amilim + betaw * (alamb * qlim - betaf) * (1 - 0.5 * (alamb * qlim + betaf));

  if ami <= amilim then
  begin
    // Single reinforcement

    // Moment resisted by flange
    amif := betaf * (1 - 0.5 * betaf);

    if ami <= amif then
    begin
      omega := 1 - Sqrt(1 - 2 * ami);
    end
    else
    begin
      ami0 := amif + (ami - amif) / betaw;
      omega := betaf * (1 - betaw) + betaw * (1 - Sqrt(1 - 2 * ami0));
    end;

    omegal := 0;
    verificacao := 'OK';

    // Determine domain
    if omega / alamb <= (eu / (eu + 10)) then
      dominio := 2
    else if omega / alamb <= qlim then
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

    // Eliminate case where qlim < delta
    if qlim < delta then
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

    omegal := (ami - amilim) * fyd / ((1 - delta) * tsl);
    omega := rcclim + (ami - amilim) / (1 - delta);

    dominio := 3;  // Double reinforcement typically in domain 3
    verificacao := 'OK - Armadura dupla';
  end;

  // Calculate reinforcement areas
  As_comp := omegal * bf * d * tcd / fyd;
  As_trac := omega * bf * d * tcd / fyd;

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

  // Cross-sectional area of T-beam
  ac := bf * hf + bw * (h - hf);
  asmin := romin * ac;

  if As_trac < asmin then
  begin
    As_trac := asmin;
    verificacao := 'OK - Armadura minima';
  end;

  // Calculate reinforcement ratio (based on total area)
  taxa := (As_trac / ac) * 100;  // Percentage
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
    WriteLn('  FLEXAO SECAO T - NBR 6118');
    WriteLn('============================================');
    WriteLn('');
    WriteLn('Geometria:');
    WriteLn('  Largura da mesa (bf):      ', bf:8:2, ' cm');
    WriteLn('  Espessura da mesa (hf):    ', hf:8:2, ' cm');
    WriteLn('  Largura da nervura (bw):   ', bw:8:2, ' cm');
    WriteLn('  Altura total (h):          ', h:8:2, ' cm');
    WriteLn('  Altura util (d):           ', d:8:2, ' cm');
    WriteLn('');
    WriteLn('Armaduras:');
    WriteLn('  Armadura tracionada (As):  ', As_trac:8:2, ' cm²');
    WriteLn('  Armadura comprimida (As''): ', As_comp:8:2, ' cm²');
    WriteLn('  Dominio:                   ', dominio);
    WriteLn('  Taxa de armadura:          ', taxa:8:4, ' %');
    WriteLn('');
    WriteLn('Momentos:');
    WriteLn('  Momento reduzido (μ):      ', ami:8:6);
    WriteLn('  Momento limite (μlim):     ', amilim:8:6);
    WriteLn('');
    WriteLn('Verificacao: ', verificacao);
    WriteLn('============================================');
    Exit;
  end;

  // Default: Concise output for AI agents
  Write('As=', As_trac:0:2);
  Write(' As''=', As_comp:0:2);
  Write(' dominio=', dominio);

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
  InitLocale;
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
