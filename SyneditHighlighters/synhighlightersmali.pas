{-------------------------------------------------------------------------------
SynHighlighterSMALI v1.0

This unit provides a Smali highlighter for SynEdit.

2025 (c) Miguel A. Risco-Castillo

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

v1.0 2025/08/25
Initial release AI assisted
-------------------------------------------------------------------------------}
{
@abstract(Provides a Smali highlighter for SynEdit)
}

{$IFNDEF QSYNHIGHLIGHTERSMALI}
unit SynHighlighterSmali;
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
  SynEditHighlighter,
{$ENDIF}
  SysUtils,
  Classes;

type
  TtkTokenKind = (tkComment, tkDirective, tkIdentifier, tkKey, tkNull,
    tkNumber, tkRegister, tkSpace, tkString, tkSymbol, tkType, tkUnknown, tkLabel);

  TRangeState = (rsUnknown);

  TProcTableProc = procedure of object;

type
  TSynSmaliSyn = class(TSynCustomHighlighter)
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
    fDirectiveAttri: TSynHighlighterAttributes;
    fNumberAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fRegisterAttri: TSynHighlighterAttributes;
    fTypeAttri: TSynHighlighterAttributes;
    fLabelAttri: TSynHighlighterAttributes;
    fKeyWords: TStringList;
    fDirectives: TStrings;

    procedure BraceOpenProc;
    procedure PointCommaProc;
    procedure CRProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure NullProc;
    procedure NumberProc;
    procedure RoundOpenProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure StringProc;
    procedure ColonProc;
    procedure HashCommentProc;
    procedure DotProc;
    procedure UnknownProc;
    procedure MakeMethodTables;
    procedure SetDirectives(const Value: TStrings);
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    function IsKeyword(const AKeyword: string): boolean; override;
    function IsDirective(aToken: string): Boolean;
    {$IFDEF SYN_LAZARUS}
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    {$ENDIF}

    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    function GetToken: string; override;
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
    property DirectiveAttri: TSynHighlighterAttributes read fDirectiveAttri
      write fDirectiveAttri;
    property DirectiveWords: TStrings read fDirectives write SetDirectives;
    property NumberAttri: TSynHighlighterAttributes read fNumberAttri
      write fNumberAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri
      write fSpaceAttri;
    property StringAttri: TSynHighlighterAttributes read fStringAttri
      write fStringAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri
      write fSymbolAttri;
    property RegisterAttri: TSynHighlighterAttributes read fRegisterAttri
      write fRegisterAttri;
    property TypeAttri: TSynHighlighterAttributes read fTypeAttri
      write fTypeAttri;
    property LabelAttri: TSynHighlighterAttributes read fLabelAttri
      write fLabelAttri;
  end;

implementation

uses
{$IFDEF SYN_CLX}
  QSynEditMiscProcs,
  QSynEditStrConst;
{$ELSE}
  SynEditStrConst, SynEditStrConstExtra;
{$ENDIF}

const
  // Smali opcodes/instructions
  SmaliKeysCount = 246;
  SmaliKeys: array[1..SmaliKeysCount] of string = (
    'NOP', 'MOVE', 'MOVE/FROM16', 'MOVE/16', 'MOVE-WIDE', 'MOVE-WIDE/FROM16',
    'MOVE-WIDE/16', 'MOVE-OBJECT', 'MOVE-OBJECT/FROM16', 'MOVE-OBJECT/16',
    'MOVE-RESULT', 'MOVE-RESULT-WIDE', 'MOVE-RESULT-OBJECT', 'MOVE-EXCEPTION',
    'RETURN-VOID', 'RETURN', 'RETURN-WIDE', 'RETURN-OBJECT', 'CONST/4',
    'CONST/16', 'CONST', 'CONST/HIGH16', 'CONST-WIDE/16', 'CONST-WIDE/32',
    'CONST-WIDE', 'CONST-WIDE/HIGH16', 'CONST-STRING', 'CONST-STRING/JUMBO',
    'CONST-CLASS', 'MONITOR-ENTER', 'MONITOR-EXIT', 'CHECK-CAST', 'INSTANCE-OF',
    'ARRAY-LENGTH', 'NEW-INSTANCE', 'NEW-ARRAY', 'FILLED-NEW-ARRAY',
    'FILLED-NEW-ARRAY/RANGE', 'FILL-ARRAY-DATA', 'THROW', 'GOTO', 'GOTO/16',
    'GOTO/32', 'PACKED-SWITCH', 'SPARSE-SWITCH', 'CMPL-FLOAT', 'CMPG-FLOAT',
    'CMPL-DOUBLE', 'CMPG-DOUBLE', 'CMP-LONG', 'IF-EQ', 'IF-NE', 'IF-LT',
    'IF-GE', 'IF-GT', 'IF-LE', 'IF-EQZ', 'IF-NEZ', 'IF-LTZ', 'IF-GEZ', 'IF-GTZ',
    'IF-LEZ', 'AGET', 'AGET-WIDE', 'AGET-OBJECT', 'AGET-BOOLEAN', 'AGET-BYTE',
    'AGET-CHAR', 'AGET-SHORT', 'APUT', 'APUT-WIDE', 'APUT-OBJECT', 'APUT-BOOLEAN',
    'APUT-BYTE', 'APUT-CHAR', 'APUT-SHORT', 'IGET', 'IGET-WIDE', 'IGET-OBJECT',
    'IGET-BOOLEAN', 'IGET-BYTE', 'IGET-CHAR', 'IGET-SHORT', 'IPUT', 'IPUT-WIDE',
    'IPUT-OBJECT', 'IPUT-BOOLEAN', 'IPUT-BYTE', 'IPUT-CHAR', 'IPUT-SHORT',
    'SGET', 'SGET-WIDE', 'SGET-OBJECT', 'SGET-BOOLEAN', 'SGET-BYTE', 'SGET-CHAR',
    'SGET-SHORT', 'SPUT', 'SPUT-WIDE', 'SPUT-OBJECT', 'SPUT-BOOLEAN', 'SPUT-BYTE',
    'SPUT-CHAR', 'SPUT-SHORT', 'INVOKE-VIRTUAL', 'INVOKE-SUPER', 'INVOKE-DIRECT',
    'INVOKE-STATIC', 'INVOKE-INTERFACE', 'INVOKE-VIRTUAL/RANGE', 'INVOKE-SUPER/RANGE',
    'INVOKE-DIRECT/RANGE', 'INVOKE-STATIC/RANGE', 'INVOKE-INTERFACE/RANGE',
    'NEG-INT', 'NOT-INT', 'NEG-LONG', 'NOT-LONG', 'NEG-FLOAT', 'NEG-DOUBLE',
    'INT-TO-LONG', 'INT-TO-FLOAT', 'INT-TO-DOUBLE', 'LONG-TO-INT', 'LONG-TO-FLOAT',
    'LONG-TO-DOUBLE', 'FLOAT-TO-INT', 'FLOAT-TO-LONG', 'FLOAT-TO-DOUBLE',
    'DOUBLE-TO-INT', 'DOUBLE-TO-LONG', 'DOUBLE-TO-FLOAT', 'INT-TO-BYTE',
    'INT-TO-CHAR', 'INT-TO-SHORT', 'ADD-INT', 'SUB-INT', 'MUL-INT', 'DIV-INT',
    'REM-INT', 'AND-INT', 'OR-INT', 'XOR-INT', 'SHL-INT', 'SHR-INT', 'USHR-INT',
    'ADD-LONG', 'SUB-LONG', 'MUL-LONG', 'DIV-LONG', 'REM-LONG', 'AND-LONG',
    'OR-LONG', 'XOR-LONG', 'SHL-LONG', 'SHR-LONG', 'USHR-LONG', 'ADD-FLOAT',
    'SUB-FLOAT', 'MUL-FLOAT', 'DIV-FLOAT', 'REM-FLOAT', 'ADD-DOUBLE', 'SUB-DOUBLE',
    'MUL-DOUBLE', 'DIV-DOUBLE', 'REM-DOUBLE', 'ADD-INT/2ADDR', 'SUB-INT/2ADDR',
    'MUL-INT/2ADDR', 'DIV-INT/2ADDR', 'REM-INT/2ADDR', 'AND-INT/2ADDR',
    'OR-INT/2ADDR', 'XOR-INT/2ADDR', 'SHL-INT/2ADDR', 'SHR-INT/2ADDR',
    'USHR-INT/2ADDR', 'ADD-LONG/2ADDR', 'SUB-LONG/2ADDR', 'MUL-LONG/2ADDR',
    'DIV-LONG/2ADDR', 'REM-LONG/2ADDR', 'AND-LONG/2ADDR', 'OR-LONG/2ADDR',
    'XOR-LONG/2ADDR', 'SHL-LONG/2ADDR', 'SHR-LONG/2ADDR', 'USHR-LONG/2ADDR',
    'ADD-FLOAT/2ADDR', 'SUB-FLOAT/2ADDR', 'MUL-FLOAT/2ADDR', 'DIV-FLOAT/2ADDR',
    'REM-FLOAT/2ADDR', 'ADD-DOUBLE/2ADDR', 'SUB-DOUBLE/2ADDR', 'MUL-DOUBLE/2ADDR',
    'DIV-DOUBLE/2ADDR', 'REM-DOUBLE/2ADDR', 'ADD-INT/LIT16', 'RSUB-INT',
    'MUL-INT/LIT16', 'DIV-INT/LIT16', 'REM-INT/LIT16', 'AND-INT/LIT16',
    'OR-INT/LIT16', 'XOR-INT/LIT16', 'ADD-INT/LIT8', 'RSUB-INT/LIT8',
    'MUL-INT/LIT8', 'DIV-INT/LIT8', 'REM-INT/LIT8', 'AND-INT/LIT8',
    'OR-INT/LIT8', 'XOR-INT/LIT8', 'SHL-INT/LIT8', 'SHR-INT/LIT8',
    'USHR-INT/LIT8', 'IGET-VOLATILE', 'IPUT-VOLATILE', 'SGET-VOLATILE',
    'SPUT-VOLATILE', 'IGET-OBJECT-VOLATILE', 'IGET-WIDE-VOLATILE',
    'IPUT-WIDE-VOLATILE', 'SGET-WIDE-VOLATILE', 'SPUT-WIDE-VOLATILE',
    'BREAKPOINT', 'THROW-VERIFICATION-ERROR', 'EXECUTE-INLINE',
    'EXECUTE-INLINE/RANGE', 'INVOKE-OBJECT-INIT/RANGE', 'RETURN-VOID-BARRIER',
    'IGET-QUICK', 'IGET-WIDE-QUICK', 'IGET-OBJECT-QUICK', 'IPUT-QUICK',
    'IPUT-WIDE-QUICK', 'IPUT-OBJECT-QUICK', 'INVOKE-VIRTUAL-QUICK',
    'INVOKE-VIRTUAL-QUICK/RANGE', 'INVOKE-SUPER-QUICK', 'INVOKE-SUPER-QUICK/RANGE',
    'IPUT-OBJECT-VOLATILE', 'SGET-OBJECT-VOLATILE', 'SPUT-OBJECT-VOLATILE');

  // Smali directives
  SmaliDirectiveCount = 21;
  SmaliDirectives: array[1..SmaliDirectiveCount] of string = (
    '.CLASS', '.SUPER', '.IMPLEMENTS', '.SOURCE', '.FIELD', '.END FIELD',
    '.SUBANNOTATION', '.END SUBANNOTATION', '.ANNOTATION', '.END ANNOTATION',
    '.ENUM', '.METHOD', '.END METHOD', '.REGISTERS', '.LOCALS', '.ARRAY-DATA',
    '.END ARRAY-DATA', '.PACKED-SWITCH', '.END PACKED-SWITCH', '.SPARSE-SWITCH',
    '.END SPARSE-SWITCH');

var
  Identifiers: array[#0..#255] of ByteBool;
  mHashTable: array[#0..#255] of Integer;

procedure MakeIdentTable;
var
  I, J: Char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '-', '/', '0'..'9', 'a'..'z', 'A'..'Z':
        Identifiers[I] := True;
      else
        Identifiers[I] := False;
    end;
    J := UpCase(I);
    case I in ['_', '-', '/', 'a'..'z', 'A'..'Z'] of
      True: mHashTable[I] := Ord(J) - 64
      else mHashTable[I] := 0;
    end;
  end;
end;

function TSynSmaliSyn.IsKeyword(const AKeyword: string): boolean;
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
end;

function TSynSmaliSyn.IsDirective(aToken: String): Boolean;
var
  First, Last, I, Compare: Integer;
  Token: String;
begin
  First := 0;
  Last := fDirectives.Count - 1;
  Result := False;
  Token := UpperCase(aToken);
  while First <= Last do
  begin
    I := (First + Last) shr 1;
    Compare := CompareStr(fDirectives[i], Token);
    if Compare = 0 then
    begin
      Result := True;
      break;
    end
    else
      if Compare < 0 then First := I + 1 else Last := I - 1;
  end;
end;

procedure TSynSmaliSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      '#': fProcTable[I] := {$ifdef FPC} @ {$endif}HashCommentProc;
      '.': fProcTable[I] := {$ifdef FPC} @ {$endif}DotProc;
      ':': fProcTable[I] := {$ifdef FPC} @ {$endif}ColonProc;
      '{': fProcTable[I] := {$ifdef FPC} @ {$endif}BraceOpenProc;
      ';': fProcTable[I] := {$ifdef FPC} @ {$endif}PointCommaProc;
      #13: fProcTable[I] := {$ifdef FPC} @ {$endif}CRProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := {$ifdef FPC} @ {$endif}IdentProc;
      #10: fProcTable[I] := {$ifdef FPC} @ {$endif}LFProc;
      #0: fProcTable[I] := {$ifdef FPC} @ {$endif}NullProc;
      '0'..'9': fProcTable[I] := {$ifdef FPC} @ {$endif}NumberProc;
      '(': fProcTable[I] := {$ifdef FPC} @ {$endif}RoundOpenProc;
      '/': fProcTable[I] := {$ifdef FPC} @ {$endif}SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := {$ifdef FPC} @ {$endif}SpaceProc;
      #34, #39: fProcTable[I] := {$ifdef FPC} @ {$endif}StringProc;
      else fProcTable[I] := {$ifdef FPC} @ {$endif}UnknownProc;
    end;
end;

constructor TSynSmaliSyn.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited Create(AOwner);

  fKeyWords := TStringList.Create;
  TStringList(fKeyWords).Sorted := True;
  TStringList(fKeyWords).Duplicates := dupIgnore;

  fDirectives := TStringList.Create;
  TStringList(fDirectives).Sorted := True;
  TStringList(fDirectives).Duplicates := dupIgnore;

  if not (csDesigning in ComponentState) then begin
    for i := 1 to SmaliKeysCount do
      fKeyWords.Add(SmaliKeys[i]);
    for i := 1 to SmaliDirectiveCount do
      fDirectives.Add(SmaliDirectives[i]);
  end;

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := clMaroon;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighlighterAttributes.Create(SYNS_AttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeyAttri := TSynHighlighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Foreground := clNavy;
  AddAttribute(fKeyAttri);

  fDirectiveAttri := TSynHighlighterAttributes.Create(SYNS_AttrDirective);
  fDirectiveAttri.Foreground := clBlue;
  fDirectiveAttri.Style := [fsBold];
  AddAttribute(fDirectiveAttri);

  fNumberAttri := TSynHighlighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clGreen;
  AddAttribute(fNumberAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fStringAttri := TSynHighlighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clOlive;
  AddAttribute(fStringAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Foreground := clBlue;
  AddAttribute(fSymbolAttri);

  fRegisterAttri := TSynHighlighterAttributes.Create('Register');
  fRegisterAttri.Foreground := clTeal;
  AddAttribute(fRegisterAttri);

  fTypeAttri := TSynHighlighterAttributes.Create('Type');
  fTypeAttri.Foreground := clPurple;
  AddAttribute(fTypeAttri);

  fLabelAttri := TSynHighlighterAttributes.Create('Label');
  fLabelAttri.Foreground := clMaroon;
  fLabelAttri.Style := [fsBold];
  AddAttribute(fLabelAttri);

  SetAttributesOnChange({$ifdef FPC} @ {$endif}DefHighlightChange);

  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterSmali;
end;

destructor TSynSmaliSyn.Destroy;
begin
  fKeyWords.Free;
  fDirectives.Free;
  inherited Destroy;
end;

{$IFDEF SYN_LAZARUS}
procedure TSynSmaliSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart  := FLine + fTokenPos;
end;
{$ENDIF}

procedure TSynSmaliSyn.SetLine(const NewValue: String; LineNumber:Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynSmaliSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSmaliSyn.PointCommaProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSmaliSyn.CRProc;
begin
  fTokenID := tkSpace;
  case FLine[Run + 1] of
    #10: inc(Run, 2);
  else inc(Run);
  end;
end;

procedure TSynSmaliSyn.ColonProc;
begin
  // Check if this is a label (something:)
  if (Run > 0) and Identifiers[FLine[Run-1]] then begin
    fTokenID := tkLabel;
  end else begin
    fTokenID := tkSymbol;
  end;
  inc(Run);
end;

procedure TSynSmaliSyn.DotProc;
begin
  inc(Run);
  // Look ahead for directive
  if Identifiers[fLine[Run]] then begin
    while Identifiers[fLine[Run]] do inc(Run);
    if IsDirective(GetToken) then
      fTokenId := tkDirective
    else
      fTokenId := tkSymbol;
  end else
    fTokenId := tkSymbol;
end;

procedure TSynSmaliSyn.HashCommentProc;
begin
  fTokenID := tkComment;
  repeat
    inc(Run);
  until fLine[Run] in [#0, #10, #13];
end;

procedure TSynSmaliSyn.IdentProc;
var
  token: string;
begin
  while Identifiers[fLine[Run]] do inc(Run);
  token := GetToken;

  // Check if this is a register (v0, p0, etc)
  if (Length(token) >= 2) and
     ((token[1] = 'v') or (token[1] = 'p')) and
     CharInSet(token[2], ['0'..'9']) then begin
    fTokenId := tkRegister;
    Exit;
  end;

  // Check if this is a type (Lpackage/name/Class;)
  if (Length(token) >= 2) and
     (token[1] = 'L') and
     (Pos('/', token) > 0) then begin
    fTokenId := tkType;
    Exit;
  end;

  if IsKeyWord(token) then begin
    fTokenId := tkKey;
  end else begin
    fTokenId := tkIdentifier;
  end;
end;

procedure TSynSmaliSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynSmaliSyn.LowerProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynSmaliSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynSmaliSyn.NumberProc;
begin
  inc(Run);
  fTokenID := tkNumber;
  while CharInSet(FLine[Run], ['0'..'9', '.', 'x', 'X', 'A'..'F', 'a'..'f']) do
    inc(Run);
end;

procedure TSynSmaliSyn.RoundOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynSmaliSyn.SlashProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynSmaliSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynSmaliSyn.StringProc;
var
  QuoteChar: Char;
begin
  fTokenID := tkString;
  QuoteChar := FLine[Run];      // either " or '
  inc(Run);
  while not (FLine[Run] in [#0, #10, #13, QuoteChar]) do
    inc(Run);
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynSmaliSyn.UnknownProc;
begin
{$IFDEF SYN_MBCSSUPPORT}
  if FLine[Run] in LeadBytes then
    Inc(Run, 2)
  else
{$ENDIF}
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynSmaliSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]];
end;

function TSynSmaliSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
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

function TSynSmaliSyn.GetEol: Boolean;
begin
  Result := False;
  if fTokenId = tkNull then Result := True;
end;

function TSynSmaliSyn.GetRange: Pointer;
begin
  Result := Pointer(PtrInt(fRange));
end;

function TSynSmaliSyn.GetToken: string;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

function TSynSmaliSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynSmaliSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  case fTokenID of
    tkComment: Result := fCommentAttri;
    tkDirective: Result := fDirectiveAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result := fKeyAttri;
    tkNumber: Result := fNumberAttri;
    tkSpace: Result := fSpaceAttri;
    tkString: Result := fStringAttri;
    tkSymbol: Result := fSymbolAttri;
    tkRegister: Result := fRegisterAttri;
    tkType: Result := fTypeAttri;
    tkLabel: Result := fLabelAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynSmaliSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynSmaliSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynSmaliSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynSmaliSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(PtrUInt(Value));
end;

procedure TSynSmaliSyn.SetDirectives(const Value: TStrings);
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
  fDirectives.Assign(Value);
  DefHighLightChange(nil);
end;

function TSynSmaliSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> 'Smali Files (*.smali)|*.smali';
end;

class function TSynSmaliSyn.GetLanguageName: string;
begin
  Result := SYNS_LangSmali;
end;

function TSynSmaliSyn.GetSampleSource: string;
begin
  Result :=
    '# Smali class file example'#13#10 +
    '.class public Lcom/example/HelloWorld;'#13#10 +
    '.super Ljava/lang/Object;'#13#10 +
    ''#13#10 +
    '.method public static main([Ljava/lang/String;)V'#13#10 +
    '    .registers 2'#13#10 +
    ''#13#10 +
    '    sget-object v0, Ljava/lang/System;->out:Ljava/io/PrintStream;'#13#10 +
    '    const-string v1, "Hello, World!"'#13#10 +
    '    invoke-virtual {v0, v1}, Ljava/io/PrintStream;->println(Ljava/lang/String;)V'#13#10 +
    ''#13#10 +
    '    return-void'#13#10 +
    '.end method';
end;

initialization
  MakeIdentTable;
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynSmaliSyn);
{$ENDIF}
end.