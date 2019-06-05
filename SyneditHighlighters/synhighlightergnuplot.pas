{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: SynHighlighterUNIXShellScript.pas, released 2001-11-13.
The Initial Author of this file is Stefan Ascher.
All Rights Reserved.
Portions by Jan Verhoeven (http://jansfreeware.com/jfdelphi.htm)

Contributors to the SynEdit and mwEdit projects are listed in the
Contributors.txt file.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the "GPL"), in which case
the provisions of the GPL are applicable instead of those above.
If you wish to allow use of your version of this file only under the terms
of the GPL and not to allow others to use your version of this file
under the MPL, indicate your decision by deleting the provisions above and
replace them with the notice and other provisions required by the GPL.
If you do not delete the provisions above, a recipient may use your version
of this file under either the MPL or the GPL.

$Id: synhighlighterunixshellscript.pas 21053 2009-08-01 10:48:48Z martin $

You may retrieve the latest version of this file at the SynEdit home page,
located at http://SynEdit.SourceForge.net

Contributors:

Tom Lisjac <vlx@users.sourceforge.net>
  Initially adapted for use with Lazarus and FPC - 2003-06-11
  Changes can be found by searching for: ////TL

Known Issues:

-------------------------------------------------------------------------------}
{
@abstract(Provides a gnuplot highlighter for SynEdit)
@author(Werner Pamler)
@created(2011, maybe)
The SynHighlighterGnuplot unit provides SynEdit with a highlighter for gnuplot.
}

{$IFNDEF QSYNHIGHLIGHTERUNIXSHELLSCRIPT}
unit synhighlightergnuplot;
{$ENDIF}

// extrasyn.inc is the synedit.inc from laz 1.2.0 synedit package source,
// If it has changed in newer version you might need to copy it again.
// Remember to redclare the syn_lazarus define.
{$I extrasyn.inc}

interface

uses
  {$IFDEF SYN_CLX}
  QGraphics,
  QSynEditTypes,
  QSynEditHighlighter,
  {$ELSE}
  Graphics,
  {$IFDEF SYN_LAZARUS}
  GraphType, /////TL 2003-06-11: Added for font attribute declaration fsBold
  {$ENDIF}
  SynEditTypes,
  SynEditHighlighter,
  {$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkIdentifier, tkKey, tkNull, tkNumber, tkSecondKey,
    tkThirdKey, tkSpace, tkString, tkSymbol, tkVariable, tkUnknown);

  TRangeState = (rsUnknown, rsAnsi, rsPasStyle, rsCStyle);

  TProcTableProc = procedure of object;

type

  { TSynGnuplotSyn }

  TSynGnuplotSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fLine: PChar;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fTokenPos: Integer;
    FTokenID: TtkTokenKind;
    fLineNumber: Integer;
    fStringAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fKeyAttri: TSynHighlighterAttributes;
    fSecondKeyAttri: TSynHighlighterAttributes;
    fThirdKeyAttri : TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fVarAttri: TSynHighlighterAttributes;
    fKeyWords: TStrings;
    fSecondKeys: TStrings;
    fThirdKeys : TStrings;
    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure UnknownProc;
    procedure MakeMethodTables;
    procedure AnsiProc;
    procedure PasStyleProc;
    procedure CStyleProc;
    procedure DollarProc;
    procedure DotProc;
    procedure SetSecondKeys(const Value: TStrings);
    procedure SetThirdKeys(const Value: TStrings);
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: String; override;
  public
    {$IFNDEF SYN_CPPB_1} class {$ENDIF}                                  
    function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine({$IFDEF FPC}const {$ENDIF}NewValue: String; LineNumber:Integer); override; /////TL: Added 2003-06-11
    function IsKeyword(const AKeyword: string): boolean; override;              //mh 2000-11-08
    function IsSecondKeyWord(aToken: string): Boolean;
    function IsThirdKeyWord(aToken: string) : boolean;
    function GetToken: string; override;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override; /////TL: Added 2003-06-11
    {$ENDIF}
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure SetRange(Value: Pointer); override;
    procedure ResetRange; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri
      write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri
      write fIdentifierAttri;
    property KeyAttri: TSynHighlighterAttributes read fKeyAttri write fKeyAttri;
    property SecondKeyAttri: TSynHighlighterAttributes read fSecondKeyAttri
      write fSecondKeyAttri;
    property SecondKeyWords: TStrings read fSecondKeys write SetSecondKeys;
    property ThirdKeyAttri: TSynHighlighterAttributes read fThirdKeyAttri
      write fThirdKeyAttri;
    property ThirdKeyWords: TStrings read fThirdKeys write SetThirdKeys;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property VarAttri: TSynHighlighterAttributes read fVarAttri
      write fVarAttri;
  end;

procedure Register;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst;
{$ENDIF}

const
  SYNS_AttrThirdReservedWord   =  'Third reserved word';
  SYNS_XML_AttrThirdReservedWord : string = SYNS_AttrThirdReservedWord;   // 'Third reserved word';

const
  GnuplotScriptKeysCount = 35; //110;
  GnuplotScriptKeys: array[1..GnuplotScriptKeysCount] of string = (
    'CALL', 'CD', 'CLEAR', 'ELSE', 'EVAL', 'EXIT', 'FIT', 'FPRINTF', 'HELP', 'HISTORY', 'IF',
    'FOR', 'LOAD', 'LOWER', 'PAUSE', 'PLOT', 'PRINT', 'PWD', 'QUIT',
    'RAISE', 'REFRESH', 'REPLOT', 'REREAD', 'RESET',
    'SAVE', 'SET', 'SHOW', 'SHELL', 'SPLOT', 'SPRINTF', 'SYSTEM',
    'TEST', 'UNDEFINE', 'UNSET', 'UPDATE');
  GnuplotScriptSecondKeysCount = 116; //23;
  GnuplotScriptSecondKeys: array[1..GnuplotScriptSecondKeysCount] of string = (
    'ANGLES', 'ARROW', 'AUTOSCALE', 'BARS', 'BIND', 'BMARGIN', 'BORDER',
    'BOXWIDTH', 'CBDATA', 'CBDTICS', 'CBLABEL', 'CBMTICS', 'CBRANGE', 'CBTICS',
    'CLABEL', 'CLIP', 'CNTRPARAM', 'COLORBOX', 'CONTOUR',
    'DECIMALSIGN', 'DGRID3D', 'DUMMY', 'ENCODING', 'FIT', 'FONTPATH',
    'FORMAT', 'FUNCTIONS', 'GRID', 'HIDDEN3D', 'HISTORYSIZE', 'ISOSAMPLES',
    'KEY', 'LABEL', 'LMARGIN', 'LOADPATH', 'LOCALE', 'LOGSCALE', 'MACROS',
    'MAPPING', 'MARGIN', 'MOUSE', 'MULTIPLOT', 'MX2TICS', 'MXTICS', 'MY2TICS',
    'MYTICS', 'MZTICS', 'OBJECT', 'OFFSETS', 'ORIGIN', 'OUTPUT', 'PARAMETRIC',
    'PLOT', 'PALETTE', 'POINTSIZE', 'POLAR', 'PRINT', 'RMARGIN', 'RRANGE',
    'SAMPLES', 'SIZE', 'STYLE', 'SURFACE', 'TABLE', 'TERMINAL', 'TERMOPTION',
    'TICS', 'TICSLEVEL', 'TICSCALE', 'TIMESTAMP', 'TIMEFMT', 'TITLE',
    'TMARGIN', 'TRANGE', 'URANGE', 'VARIABLES', 'VERSION', 'VIEW', 'VRANGE',
    'X2DATA', 'X2DTICS', 'X2LABEL', 'X2MTICS', 'X2RANGE', 'X2TICS', 'X2ZEROAXIS',
    'XDATA', 'XDTICS', 'XLABEL', 'XMTICS', 'XRANGE', 'XTICS', 'XYPLANE',
    'XZEROAXIS', 'Y2DATE', 'Y2DTICS', 'Y2LABEL', 'Y2MTCIS', 'Y2RANGE', 'Y2TICS',
    'Y2ZEROAXIS', 'YDATA', 'YDTICS', 'YLABEL', 'YMTICS', 'YRANGE', 'YTICS',
    'YZEROAXIS', 'ZDATA', 'ZDTICS', 'ZERO', 'ZLABEL', 'ZMTICS', 'ZRANGE',
    'ZTICS', 'ZZEROAXIS');

  GnuplotScriptThirdKeysCount = 134;
  GnuplotScriptThirdKeys: array[1..GnuplotScriptThirdKeysCount] of string = (
    'absolute', 'all', 'as', 'auto',
    'back', 'backhead', 'base', 'bdefault', 'binary', 'border', 'both',
    'box', 'boxes', 'boxerrorbars', 'boxxyerrorbars', 'bspline',
    'candlesticks', 'cauchy', 'circles', 'column', 'columnheader', 'cubicspline',
    'default', 'defined', 'discrete', 'dots',
    'else', 'empty', 'end', 'errorbars', 'errorlines', 'errorvariables', 'every', 'exp',
    'fill', 'filled', 'filledcurve', 'financebars', 'fix', 'fixmax', 'fixmin',
    'font', 'from', 'front', 'fs', 'fsteps', 'fullwidth',
    'keepfix',
    'gauss',
    'hann', 'head', 'heads', 'histeps', 'histograms', 'horizontal',
    'image', 'impulses', 'in', 'incremental', 'index',
    'labels', 'large', 'levels', 'linear', 'lines',
    'linecolor', 'linespoints', 'linestyle', 'linetype', 'linewidth',
    'locale', 'lc', 'ls', 'lt', 'lw',
    'matrix', 'max', 'min', 'missing',
    'noborder', 'nocontours', 'noerrorvariables', 'nohead', 'nohidden3d',
    'nosurface', 'notitle',
    'offset', 'order', 'origin',
    'pm3d', 'points', 'pointsize', 'pointtype', 'ps', 'pt',
    'qnorm',
    'radians', 'relative', 'rgb', 'rgbformulae', 'rgbalpha', 'rgbimage',
    'rotate by', 'rto',
    'size', 'small', 'smooth', 'splines', 'square', 'steps', 'surface',
    'textcolor', 'thru', 'to', 'trianglepattern',
    'user', 'using',
    'vectors', 'vertical', 'volatile',
    'with',
    'x', 'x1', 'x2', 'xerrorbar', 'xerrorlines', 'xyerrorbars', 'xyerrorlines',
    'y', 'y1', 'y2', 'yerrorbars', 'yerrorlines',
    'z'
  );

var
  Identifiers: array[#0..#255] of ByteBool;
  {$IFNDEF SYN_LAZARUS}
  mHashTable: array[#0..#255] of Integer;
  {$ENDIF}

procedure MakeIdentTable;
var
  I: Char;
  {$IFNDEF SYN_LAZARUS}
  K: Char;
  {$ENDIF}
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z':
        Identifiers[I] := True;
      else
        Identifiers[I] := False;
    end;
    {$IFNDEF SYN_LAZARUS}
    J := UpCase(I);
    case I in ['_', 'a'..'z', 'A'..'Z'] of
      True: mHashTable[I] := Ord(J) - 64
      else mHashTable[I] := 0;
    end;
    {$ENDIF}
  end;
end;

function TSynGnuplotSyn.IsKeyword(const AKeyword: string): boolean;                //mh 2000-11-08
var
  First, Last, I, Compare: Integer;
  Token: String;
begin
  First := 0;
  Last := fKeywords.Count - 1;
  Result := False;
  Token := UpperCase(AKeyword);

  while First <= Last do begin
    I := (First + Last) shr 1;
    Compare := CompareStr(fKeywords[I], Token);
    if Compare = 0 then begin
      Result := True;
      break;
    end else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsKeyWord }

function TSynGnuplotSyn.IsSecondKeyWord(aToken: String): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: String;
begin
  First := 0;
  Last := fSecondKeys.Count - 1;
  Result := False;
  Token := UpperCase(aToken);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := CompareStr(fSecondKeys[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsSecondKeyWord }

function TSynGnuplotSyn.IsThirdKeyWord(aToken: String): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: String;
begin
  First := 0;
  Last := fThirdKeys.Count - 1;
  Result := False;
  Token := UpperCase(aToken);
  while First <= Last do begin
    I := (First + Last) shr 1;
    Compare := CompareStr(fThirdKeys[i], Token);
    if Compare = 0 then begin
      Result := True;
      break;
    end else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end; { IsThirdKeyWord }

procedure TSynGnuplotSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of  /////TL 2003-06-11: added "@" prefix to function identifiers being assigned
      '#': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}SlashProc{!@#$AsciiCharProc};
      '{': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}BraceOpenProc;
      ';': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}PointCommaProc;
      '.': fProcTable[i] := {$IFDEF SYN_LAZARUS}@{$ENDIF}DotProc;
      #13: fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}CRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}IdentProc;
      #10: fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}LFProc;
      #0: fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}NullProc;
      '0'..'9': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}NumberProc;
      '(': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}RoundOpenProc;
      '/': fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}SlashProc;
      '$': fProcTable[i] := {$IFDEF SYN_LAZARUS}@{$ENDIF}DollarProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}SpaceProc;
      #34, #39{!@#$#39}: fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}StringProc;
      else fProcTable[I] := {$IFDEF SYN_LAZARUS}@{$ENDIF}UnknownProc;
    end;
end;

constructor TSynGnuplotSyn.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited Create(AOwner);
  fKeyWords := TStringList.Create;
  TStringList(fKeyWords).Sorted := True;
  TStringList(fKeyWords).Duplicates := dupIgnore;
  fSecondKeys := TStringList.Create;
  TStringList(fSecondKeys).Sorted := True;
  TStringList(fSecondKeys).Duplicates := dupIgnore;
  fThirdKeys := TStringList.Create;
  TStringList(fThirdKeys).Sorted := True;
  TStringList(fThirdKeys).Duplicates := dupIgnore;
  if not (csDesigning in ComponentState) then begin
    for i := 1 to GnuplotScriptKeysCount do
      fKeyWords.Add(GnuplotScriptKeys[i]);
    for i := 1 to GnuplotScriptSecondKeysCount do
      fSecondKeys.Add(GnuplotScriptSecondKeys[i]);
    for i := 1 to GnuplotScriptThirdKeysCount do
      fThirdKeys.Add(Uppercase(GnuplotScriptThirdKeys[i]));
  end;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment, SYNS_XML_AttrComment);
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);
  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier, SYNS_XML_AttrIdentifier);
  AddAttribute(fIdentifierAttri);
  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord, SYNS_XML_AttrReservedWord);
  fKeyAttri.Foreground := clNavy;
  fKeyAttri.Style := [fsBold];
  AddAttribute(fKeyAttri);
  fSecondKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrSecondReservedWord, SYNS_XML_AttrSecondReservedWord);
  AddAttribute(fSecondKeyAttri);
  fThirdKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrThirdReservedWord, SYNS_XML_AttrThirdReservedWord);
  AddAttribute(fThirdKeyAttri);
  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber, SYNS_XML_AttrNumber);
  fNumberAttri.Foreground := clGreen; //Blue;
  AddAttribute(fNumberAttri);
  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace, SYNS_XML_AttrSpace);
  AddAttribute(fSpaceAttri);
  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString, SYNS_XML_AttrString);
  fStringAttri.Foreground := clMaroon;
  AddAttribute(fStringAttri);
  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol, SYNS_XML_AttrSymbol);
  fSymbolAttri.Foreground := clRed;
  AddAttribute(fSymbolAttri);
  fVarAttri := TSynHighlighterAttributes.Create(SYNS_AttrVariable, SYNS_XML_AttrVariable);
  fVarAttri.Foreground := clPurple;
  AddAttribute(fVarAttri);
  SetAttributesOnChange({$IFDEF SYN_LAZARUS}@{$ENDIF}DefHighlightChange);   ////TL 2003-06-11: added the @prefix to DefHighlightChange

  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterUNIXShellScript;
end; { Create }

procedure TSynGnuplotSyn.SetLine(
  {$IFDEF FPC}const {$ENDIF}NewValue: String;
  LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end; { SetLine }

destructor TSynGnuplotSyn.Destroy;
begin
  fKeyWords.Free;
  fSecondKeys.Free;
  fThirdKeys.Free;
  inherited Destroy;
end; { Destroy }

procedure TSynGnuplotSyn.AnsiProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;

    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while fLine[Run] <> #0 do
    case fLine[Run] of
      '*':
        if fLine[Run + 1] = ')' then
        begin
          fRange := rsUnKnown;
          inc(Run, 2);
          break;
        end else inc(Run);
      #10: break;

      #13: break;
    else inc(Run);
    end;
end;

procedure TSynGnuplotSyn.PasStyleProc;
begin
  fTokenID := tkComment;
  case FLine[Run] of
    #0:
      begin
        NullProc;
        exit;
      end;
    #10:
      begin
        LFProc;
        exit;
      end;

    #13:
      begin
        CRProc;
        exit;
      end;
  end;

  while FLine[Run] <> #0 do
    case FLine[Run] of
      '}':
        begin
          fRange := rsUnKnown;
          inc(Run);
          break;
        end;
      #10: break;

      #13: break;
    else inc(Run);
    end;
end;

procedure TSynGnuplotSyn.CStyleProc;
begin
  case fLine[Run] of
    #0: NullProc;
    #10: LFProc;
    #13: CRProc;
  else
    fTokenID := tkComment;
    repeat
      if (fLine[Run] = '*') and (fLine[Run + 1] = '/') then
      begin
        fRange := rsUnKnown;
        Inc(Run, 2);
        break;
      end;
      Inc(Run);
    until fLine[Run] in [#0, #10, #13];
  end;
end;

procedure TSynGnuplotSyn.DollarProc;
var
  cc: Char;
begin
  inc(Run);
  fTokenID := tkVariable;
  if FLine[Run] = #0 then Exit;
  cc := FLine[Run];
  inc(Run);
  if (cc = '{') then begin
    // ${var}
    while FLine[Run] in IdentChars do begin
      case FLine[Run] of
        #0, #10, #13: Break;
      end;
      inc(Run);
    end;
    if FLine[Run] = '}' then Inc(Run);
  end else
    // $var
    while FLine[Run] in IdentChars do
      inc(Run);
end;

procedure TSynGnuplotSyn.DotProc;
  function TestDot: boolean;
  var
    i: integer;
  begin
    result := false;
    i := run;
    inc(i);
    while (FLine[i] in ['a'..'z', 'A'..'Z']) do
      inc(i);
    if i > (run + 1) then
      result := true;
    if result then
      run := i;
  end;
begin
  // Don't highlight filenames like filename.zip
  if TestDot then
    fTokenID := tkIdentifier
  else begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynGnuplotSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynGnuplotSyn.PointCommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynGnuplotSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynGnuplotSyn.IdentProc;
begin
  while Identifiers[fLine[Run]] do inc(Run);
  if IsKeyWord(GetToken) then begin
    fTokenId := tkKey;
    Exit;
  end else
    fTokenId := tkIdentifier;

  if IsSecondKeyWord(GetToken) then begin
    fTokenId := tkSecondKey;
    exit;
  end else
    fTokenId := tkIdentifier;

  if IsThirdKeyword(GetToken) then
    fTokenID := tkThirdKey
  else
    fTokenId := tkIdentifier;
end;

procedure TSynGnuplotSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynGnuplotSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynGnuplotSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'e', 'E'] do
  begin
    case FLine[Run] of
      '.':
        if FLine[Run + 1] = '.' then break;
    end;
    inc(Run);
  end;
end;

procedure TSynGnuplotSyn.RoundOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynGnuplotSyn.SlashProc;
begin
  if FLine[Run] = '#' then begin
    // Perl Styled Comment
    inc(Run);
    fTokenID := tkComment;
    while FLine[Run] <> #0 do
    begin
      case FLine[Run] of
        #10, #13: break;
      end;
      inc(Run);
    end;
  end else begin
    inc(Run);
    fTokenID := tkSymbol;
  end;
end;

procedure TSynGnuplotSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynGnuplotSyn.StringProc;
var
  QuoteChar: Char;
begin
// Single and Double Quotes.

  fTokenID := tkString;
  QuoteChar := FLine[Run];      // either " or '
  if (FLine[Run + 1] = QuoteChar) and (FLine[Run + 2] = QuoteChar)
    then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
    end;
    inc(Run);
  until FLine[Run] = QuoteChar;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynGnuplotSyn.UnknownProc;
begin
  inc(Run);
 {$IFDEF SYN_LAZARUS}
  while (fLine[Run] in [#128..#191]) or  // continued utf8 subcode
        ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
 {$ENDIF}
  fTokenID := tkUnKnown;
end;

procedure TSynGnuplotSyn.Next;
begin
  fTokenPos := Run;
  case fRange of
    rsAnsi: AnsiProc;
    rsPasStyle: PasStyleProc;
    rsCStyle: CStyleProc;
  else
    fProcTable[fLine[Run]];
  end;
end;

function TSynGnuplotSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result := fKeyAttri;
    SYN_ATTR_STRING: Result := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
  else
    Result := nil;
  end;
end;

function TSynGnuplotSyn.GetEol: Boolean;
begin
  Result := False;
  if fTokenId = tkNull then Result := True;
end;

function TSynGnuplotSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynGnuplotSyn.GetToken: string;
var
  Len: LongInt;
begin
  Result := '';
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

////TL 2003-06-11: Added the following to satisfy abstract method override
{$IFDEF SYN_LAZARUS}
procedure TSynGnuplotSyn.GetTokenEx(out TokenStart: PChar;
  out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart:=FLine + fTokenPos;
end;
{$ENDIF}

function TSynGnuplotSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynGnuplotSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkSecondKey: Result := fSecondKeyAttri;
    tkThirdKey : result := fThirdKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkVariable: Result := fVarAttri;
    tkUnknown: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynGnuplotSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynGnuplotSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynGnuplotSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynGnuplotSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrUInt(Value));
end;

function TSynGnuplotSyn.GetSampleSource: String;
begin
  Result :=
    '#' + LineEnding +
    '# gnuplot sample source'+ LineEnding +
    '# see: http://gnuplot.sourceforge.net/demo_5.0/multiaxis.html' + LineEnding +
    '#' + LineEnding +
    '# Use the 3rd plot of the electronics demo to show off' + LineEnding +
    '# the use of multiple x and y axes in the same plot.'+ LineEnding +
    '#' + LineEnding +
    'A(jw) = ({0,1}*jw/({0,1}*jw+p1)) * (1/(1+{0,1}*jw/p2))' + LineEnding +
    'p1 = 10' + LineEnding +
    'p2 = 10000' + LineEnding +
    'set dummy jw' + LineEnding +
    'set grid x y2' + LineEnding +
    'set key center top title " "' + LineEnding +
    'set logscale xy' + LineEnding +
    'set log x2' + LineEnding +
    'unset log y2' + LineEnding +
    'set title "Transistor Amplitude and Phase Frequency Response"' + LineEnding +
    'set xlabel "jw (radians)"' + LineEnding +
    'set xrange [1.1 : 90000.0]' + LineEnding +
    'set x2range [1.1 : 90000.0]' + LineEnding +
    'set ylabel "magnitude of A(jw)"' + LineEnding +
    'set y2label "Phase of A(jw) (degrees)"' + LineEnding +
    'set ytics nomirror' + LineEnding +
    'set y2tics' + LineEnding +
    'set tics out' + LineEnding +
    'set autoscale  y' + LineEnding +
    'set autoscale y2' + LineEnding +
    'plot abs(A(jw)) axes x1y1, 180./pi*arg(A(jw)) axes x2y2';
end;

procedure TSynGnuplotSyn.SetSecondKeys(const Value: TStrings);
var
  i: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for i := 0 to Value.Count - 1 do
        Value[i] := UpperCase(Value[i]);
      Value.EndUpdate;
    end;
  fSecondKeys.Assign(Value);
  DefHighLightChange(nil);
end;

procedure TSynGnuplotSyn.SetThirdKeys(const Value: TStrings);
var
  i: Integer;
begin
  if Value <> nil then
    begin
      Value.BeginUpdate;
      for i := 0 to Value.Count - 1 do
        Value[i] := UpperCase(Value[i]);
      Value.EndUpdate;
    end;
  fThirdKeys.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynGnuplotSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

resourcestring
  LangName = 'Gnuplot Script';

{$IFNDEF SYN_CPPB_1} class {$ENDIF}
function TSynGnuplotSyn.GetLanguageName: string;
begin
  Result := LangName;
end;

procedure Register;
begin
  RegisterComponents('SynEdit', [TSynGnuplotSyn]);
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynGnuplotSyn);
{$ENDIF}
end.

