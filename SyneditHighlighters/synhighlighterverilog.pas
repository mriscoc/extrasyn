{-------------------------------------------------------------------------------
SynHighlighterVerilog v1.2

This unit provides a Verilog highlighter for SynEdit.

2015 (c) Miguel A. Risco-Castillo

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/
Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

Some concepts in this unit are based in the Original Code of Zhou Kan and
"La Biblia del SynEdit" of Tito Hinostroza.
All Rights Reserved.

v1.2 2019/12/21
Adapted to extrasyn package

v1.1 2015
Add special list of highlighted words

-------------------------------------------------------------------------------}

unit synhighlighterverilog;

// extrasyn.inc is the synedit.inc from laz 1.2.0 synedit package source,
// If it has changed in newer version you might need to copy it again.
// Remember to redclare the syn_lazarus define.
{$I extrasyn.inc}
{$H+}

interface

uses
  SysUtils, Classes, Graphics, SynEditStrConstExtra, SynEditHighlighterFoldBase, SynEditHighlighter, SynEditTypes, SynEditStrConst;

type
  TtkTokenKind = (tkSBA, tkIeee, tkComment, tkIdentifier, tkKey, tkNull,
    tkNumber, tkSpace, tkString, tkSymbol, tkAttribute, tkUnknown);

  TRangeState = (rsUnknown, rsComment);

  TProcTableProc = procedure of object;

type

  { TSynVerilogSyn }

  TSynVerilogSyn = class(TSynCustomFoldHighlighter)
  private
    FHLWordsList: TStringList;
    fRange: TRangeState;
    fLine: PChar;
    fLineNumber: integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: longint;
    fStringLen: integer;
    fToIdent: PChar;
    fTokenPos: integer;
    FTokenID: TtkTokenKind;
    fCommentAttri: TSynHighLighterAttributes;
    fIdentifierAttri: TSynHighLighterAttributes;
    fKeyAttri: TSynHighLighterAttributes;
    fNumberAttri: TSynHighLighterAttributes;
    fSpaceAttri: TSynHighLighterAttributes;
    fStringAttri: TSynHighLighterAttributes;
    fSymbolAttri: TSynHighLighterAttributes;
    fAttribAttri: TSynHighLighterAttributes;
    FIeeeAttri: TSynHighLighterAttributes;
    fSBAAttri: TSynHighLighterAttributes;
    fDivider:TSynDividerDrawConfigSetting;
    procedure CommentProc;
    function KeyComp(const aKey: string): boolean;
    procedure SetHLWordsList(AValue: TStringList);
    procedure SymbolsProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure CRProc;
    procedure ColonProc;
    procedure EqualProc;
    procedure EndTokenProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure ModSymbolProc;
    procedure NotSymbolProc;
    procedure NullProc;
    procedure NumberProc;
    procedure OrSymbolProc;
    procedure PlusProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure TildeProc;
    procedure AttribProc;
    procedure XOrSymbolProc;
    procedure UnknownProc;
    procedure FoldValidProc;
    procedure MakeMethodTables;
  protected
    function GetIdentChars: TSynIdentChars; override;
    function GetSampleSource: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
      override;
    function GetEOL: boolean; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber:Integer); override;
    function GetToken: string; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighLighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: integer; override;
    procedure Next; override;
    function GetRange: Pointer; override;
    procedure SetRange(Value: Pointer); override;
    procedure ReSetRange; override;
    function GetDrawDivider(Index: integer): TSynDividerDrawConfigSetting; override;
    property IdentChars;
    class function GetLanguageName: string; override;
  published
    property CommentAttri: TSynHighLighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighLighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeyAttri: TSynHighLighterAttributes read fKeyAttri write fKeyAttri;
    property NumberAttri: TSynHighLighterAttributes read fNumberAttri write fNumberAttri;
    property SpaceAttri: TSynHighLighterAttributes read fSpaceAttri write fSpaceAttri;
    property StringAttri: TSynHighLighterAttributes read fStringAttri write fStringAttri;
    property SymbolAttri: TSynHighLighterAttributes read fSymbolAttri write fSymbolAttri;
    property AttribAttri: TSynHighLighterAttributes read fAttribAttri write fAttribAttri;
    property IeeeAttri: TSynHighLighterAttributes read fIeeeAttri write fIeeeAttri;
    property SBAAttri: TSynHighLighterAttributes read fSBAAttri write fSBAAttri;
    property HLWordsList: TStringList read FHLWordsList write SetHLWordsList;
  end;

implementation

var
  Identifiers: array[#0..#255] of bytebool;
  mHashTable: array[#0..#255] of integer;

procedure MakeIdentTable;
var
  I, J: char;
begin
  for I := #0 to #255 do
  begin
    case I of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[I] := True;
      else 
        Identifiers[I] := False;
    end;
    J := I; //UpCase(I); In verilog the keywords are in lower case
    case I in ['_', '0'..'9', 'A'..'Z', 'a'..'z'] of
      True: mHashTable[I] := Ord(J)
        else
          mHashTable[I] := 0;
    end;
  end;
end;

function TSynVerilogSyn.KeyComp(const aKey: string): boolean;
var
  I: integer;
  Temp: PChar;
begin
  Temp := fToIdent;   // Test from the second char +1
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do   // Test from the second char := 2
    begin
      if mHashTable[Temp^] <> mHashTable[aKey[i]] then
      begin
        Result := False;
        break;
      end;
      inc(Temp);
    end;
  end 
  else 
    Result := False;
end;

procedure TSynVerilogSyn.SetHLWordsList(AValue: TStringList);
begin
  if FHLWordsList=AValue then Exit;
  FHLWordsList:=AValue;
end;

procedure TSynVerilogSyn.IdentProc;
var i:integer;
begin
  while Identifiers[fLine[Run]] do inc(Run);
  fStringLen:=Run-fTokenPos;
  fToIdent:=fLine + fTokenPos;
  fTokenID := tkUnknown;
  if FHLWordsList.Count>0 then for i:=0 to FHLWordsList.Count-1 do
  begin
    if KeyComp(FHLWordsList[i]) then fTokenID := tkString;
  end;
  case Upcase(fToIdent^) of
    'A':begin
      //verilog
      if KeyComp('always') or
         KeyComp('and') or
         KeyComp('assign') or
         KeyComp('automatic')
      then fTokenID := tkKey;
    end;
    'B':begin
      //verilog
      if KeyComp('begin') or
         KeyComp('buf') or
         KeyComp('bufif0') or
         KeyComp('bufif1')
      then fTokenID := tkKey;
      if KeyComp('begin') then begin fTokenID := tkKey; StartCodeFoldBlock(nil); end;
    end;
    'C':begin
      //verilog
      if KeyComp('cell') or
         KeyComp('cmos') or
         KeyComp('config')
      then fTokenID := tkKey;
      if KeyComp('case') then begin fTokenID := tkKey; StartCodeFoldBlock(nil); end;
      if KeyComp('casex') then begin fTokenID := tkKey; StartCodeFoldBlock(nil); end;
      if KeyComp('casez') then begin fTokenID := tkKey; StartCodeFoldBlock(nil); end;
    end;
    'D':begin
      //verilog
      if KeyComp('deassign') or
         KeyComp('default') or
         KeyComp('defparam') or
         KeyComp('design') or
         KeyComp('disable') then fTokenID := tkKey;
    end;
    'E':begin
      //verilog
      if KeyComp('edge') or
         KeyComp('else') or
         KeyComp('event')
      then fTokenID := tkKey;
      if KeyComp('end') then endTokenProc;
      if KeyComp('endcase') then endTokenProc;
      if KeyComp('endconfig') then endTokenProc;
      if KeyComp('endfunction') then endTokenProc;
      if KeyComp('endgenerate') then endTokenProc;
      if KeyComp('endmodule') then endTokenProc;
      if KeyComp('endprimitive') then endTokenProc;
      if KeyComp('endspecify') then endTokenProc;
      if KeyComp('endtable') then endTokenProc;
      if KeyComp('endtask') then endTokenProc;
    end;
    'F':begin
      //verilog
      if KeyComp('for') or
         KeyComp('force') or
         KeyComp('forever') or
         KeyComp('fork') or
         KeyComp('function')
      then fTokenID := tkKey;
    end;
    'G':begin
      //verilog
      if KeyComp('generate') or
         KeyComp('genvar')
      then fTokenID := tkKey;
    end;
    'H':begin
      //verilog
      if KeyComp('highz0') or
         KeyComp('highz1')
      then fTokenID := tkKey;
    end;
    'I':begin
      //verilog
      if KeyComp('if') or
         KeyComp('ifnone') or
         KeyComp('incdir') or
         KeyComp('include') or
         KeyComp('initial') or
         KeyComp('inout') or
         KeyComp('input') or
         KeyComp('instance') or
         KeyComp('integer')
      then fTokenID := tkKey;
    end;
    'J':begin
      if KeyComp('join') then fTokenID := tkKey;
    end;
    'K':begin

    end;
    'L':begin
      //verilog
      if KeyComp('large') or
         KeyComp('liblist') or
         KeyComp('library') or
         KeyComp('localparam')
      then fTokenID := tkKey;
    end;
    'M':begin
      //verilog
      if KeyComp('macromodule') or
         KeyComp('medium')
      then fTokenID := tkKey;
      if KeyComp('module') then begin fTokenID := tkKey; StartCodeFoldBlock(nil); end;
    end;
    'N':begin
      //verilog
      if KeyComp('nand') or
         KeyComp('negedge') or
         KeyComp('nmos') or
         KeyComp('nor') or
         KeyComp('noshowcancelled') or
         KeyComp('not') or
         KeyComp('notif0') or
         KeyComp('notif1')
      then fTokenID := tkKey;
    end;
    'O':begin
      //verilog
      if KeyComp('or') or
         KeyComp('output')
      then fTokenID := tkKey;
    end;
    'P':begin
      //verilog
      if KeyComp('parameter') or
         KeyComp('pmos') or
         KeyComp('posedge') or
         KeyComp('primitive') or
         KeyComp('pull0') or
         KeyComp('pull1') or
         KeyComp('pulldown') or
         KeyComp('pullup') or
         KeyComp('pulsestyle_oneventglitch') or
         KeyComp('pulsestyle_ondetectglitch')
      then fTokenID := tkKey;
      //if KeyComp('process') or
      //   KeyComp('package') then begin fTokenID := tkKey; StartCodeFoldBlock(nil); end;
      //if KeyComp('procedure') then begin fTokenID := tkKey; FoldValidProc; end;
    end;
    'Q':begin

    end;
    'R':begin
      //verilog
      if KeyComp('remos') or
         KeyComp('real') or
         KeyComp('realtime') or
         KeyComp('reg') or
         KeyComp('release') or
         KeyComp('repeat') or
         KeyComp('rnmos') or
         KeyComp('rpmos') or
         KeyComp('rtran') or
         KeyComp('rtranif0') or
         KeyComp('rtranif1')
      then fTokenID := tkKey;
    end;
    'S':begin
      //verilog
      if KeyComp('scalared') or
         KeyComp('showcancelled') or
         KeyComp('signed') or
         KeyComp('small') or
         KeyComp('specify') or
         KeyComp('specparam') or
         KeyComp('strong0') or
         KeyComp('strong1') or
         KeyComp('supply0') or
         KeyComp('supply1')
      then fTokenID := tkKey;
      //SBA
      if KeyComp('sbaread') or
         KeyComp('sbawrite') or
         KeyComp('sba_config') or
         KeyComp('sba_package') or
         KeyComp('sba_typedef') or
         KeyComp('sbacall') or
         KeyComp('sbaret') or
         KeyComp('sbajmp')
      then fTokenID := tkSBA;
    end;
    'T':begin
      //verilog
      if KeyComp('table') or
         KeyComp('task') or
         KeyComp('time') or
         KeyComp('tran') or
         KeyComp('tranif0') or
         KeyComp('tranif1') or
         KeyComp('tri') or
         KeyComp('tri0') or
         KeyComp('tri1') or
         KeyComp('triand') or
         KeyComp('trior') or
         KeyComp('trireg')
      then fTokenID := tkKey;
    end;
    'U':begin
      //verilog
      if KeyComp('unsigned') or
         KeyComp('use')
      then fTokenID := tkKey;
    end;
    'V':begin
      //verilog
      if KeyComp('vectored') then fTokenID := tkKey;
    end;
    'W':begin
      //verilog
      if KeyComp('wait') or
         KeyComp('wand') or
         KeyComp('weak0') or
         KeyComp('weak1') or
         KeyComp('while') or
         KeyComp('wire') or
         KeyComp('wor')
      then fTokenID := tkKey;
//      if KeyComp('while') then begin fTokenID := tkKey; StartCodeFoldBlock(nil); end;
    end;
    'X':begin
      //verilog
      if KeyComp('xnor') or
         KeyComp('xor')
      then fTokenID := tkKey;
    end;
    'Y':begin

    end;
  end;
end;

procedure TSynVerilogSyn.MakeMethodTables;
var
  I: char;
begin
  for I := #0 to #255 do
    case I of
      #0 : fProcTable[I]      := @NullProc;
      '#','$','&','@' : fProcTable[I]  := @SymbolsProc;
      '}': fProcTable[I]      := @BraceCloseProc;
      '{': fProcTable[I]      := @BraceOpenProc;
      #13: fProcTable[I]      := @CRProc;
      ':': fProcTable[I]      := @ColonProc;
      '=': fProcTable[I]      := @EqualProc;
      '>': fProcTable[I]      := @GreaterProc;
      'A'..'Z', 'a'..'z', '_': fProcTable[I] := @IdentProc;
      #10: fProcTable[I]      := @LFProc;
      '<': fProcTable[I]      := @LowerProc;
      '-': fProcTable[I]      := @MinusProc;
      '%': fProcTable[I]      := @ModSymbolProc;
      '!': fProcTable[I]      := @NotSymbolProc;
      '0'..'9': fProcTable[I] := @NumberProc;
      '|': fProcTable[I]      := @OrSymbolProc;
      '+': fProcTable[I]      := @PlusProc;
      '.': fProcTable[I]      := @PointProc;
      ')': fProcTable[I]      := @RoundCloseProc;
      '(': fProcTable[I]      := @RoundOpenProc;
      ';': fProcTable[I]      := @SemiColonProc;
      '/': fProcTable[I]      := @SlashProc;
      #1..#9, #11, #12, #14..#32: fProcTable[I] := @SpaceProc;
      ']': fProcTable[I]      := @SquareCloseProc;
      '[': fProcTable[I]      := @SquareOpenProc;
      '*': fProcTable[I]      := @StarProc;
      #34: fProcTable[I]      := @StringProc;     //"
      '~': fProcTable[I]      := @TildeProc;
      #39: fProcTable[I]      := @AttribProc;       //'
      #96: fProcTable[I]      := @AttribProc;       //`
      '^': fProcTable[I]      := @XOrSymbolProc;
      else 
        fProcTable[I] := @UnknownProc;
    end;
end;

constructor TSynVerilogSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  
  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);
  
  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier);
  fIdentifierAttri.Foreground := clWindowText;
  AddAttribute(fIdentifierAttri);
  
  fKeyAttri := TSynHighLighterAttributes.Create(SYNS_AttrReservedWord);
  fKeyAttri.Foreground := clBlue;
  AddAttribute(fKeyAttri);
  
  fNumberAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumber);
  fNumberAttri.Foreground := clRed;
  AddAttribute(fNumberAttri);
  
  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrWhitespace);
  AddAttribute(fSpaceAttri);
  
  fStringAttri := TSynHighLighterAttributes.Create(SYNS_AttrString);
  fStringAttri.Foreground := clMaroon;
  AddAttribute(fStringAttri);
  
  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol);
  fSymbolAttri.Foreground := clTeal;
  AddAttribute(fSymbolAttri);
  
  fAttribAttri := TSynHighLighterAttributes.Create(SYNS_AttrAttributeName);
  fAttribAttri.Foreground := $000080FF;
  AddAttribute(fAttribAttri);

  fIeeeAttri := TSynHighLighterAttributes.Create(SYNS_AttrSecondReservedWord);
  fIeeeAttri.Foreground := $00804000;
  AddAttribute(fIeeeAttri);

  fSBAAttri := TSynHighLighterAttributes.Create(SYNS_AttrMiscellaneous);
  fSBAAttri.Foreground := $00C08000;
  AddAttribute(fSBAAttri);

  SetAttributesOnChange(@DefHighlightChange);
  MakeMethodTables;
  fRange := rsUnknown;
  fDefaultFilter := SYNS_FilterVerilog;

  fDivider.Color:=clNone;
  FHLWordsList:=TStringList.Create;
end;

destructor TSynVerilogSyn.Destroy;
begin
  FreeAndNil(FHLWordsList);
  inherited Destroy;
end;

procedure TSynVerilogSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  inherited;
  fLine       := PChar(NewValue);
  Run         := 0;
  fLineNumber := LineNumber;
  Next;
end;

procedure TSynVerilogSyn.SymbolsProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVerilogSyn.BraceCloseProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVerilogSyn.BraceOpenProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVerilogSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run + 1] = #10 then Inc(Run);
end;

procedure TSynVerilogSyn.ColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.EqualProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.GreaterProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynVerilogSyn.LowerProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.MinusProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.SlashProc;
begin
  fTokenID := tkSymbol;
  inc(Run);

  if (FLine[Run] = '/') then
  begin
    if (FLine[Run+1] = ' ') and (FLine[Run+2] = '/') and
       (FLine[Run+3] = 'L') and (FLine[Run+4] = ':') then
    begin
       fTokenID := tkSBA;
    end else fTokenID := tkComment;
    while not (FLine[Run] in [#0, #10, #13]) do inc(Run);
  end else if (FLine[Run] = '*') then
  begin
    fRange := rsComment; //marca inicio de rango comentario
    CommentProc; //Procesa en modo rango comentario
  end;
end;

procedure TSynVerilogSyn.CommentProc;
begin
  fTokenID := tkComment;
  if (FLine[Run]=#0) then begin NullProc; exit; end; // Si fin de línea
  while not (FLine[Run] in [#0, #10, #13]) do
  begin
    if (FLine[Run]='*') and (FLine[Run+1]='/') then
    begin
      fRange:=rsUnknown;
      inc(Run,2);
      break;
    end else inc(Run);
  end;
end;

procedure TSynVerilogSyn.ModSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVerilogSyn.NotSymbolProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynVerilogSyn.NumberProc;
begin
  fTokenID := tkNumber;
  while FLine[Run] in ['0'..'9', '.', 'u', 'U', 'l', 'L', 'x', 'X', 'e', 'E', 'f', 'F'] do
    inc(Run);
end;

procedure TSynVerilogSyn.OrSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVerilogSyn.PlusProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.PointProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.RoundCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.RoundOpenProc;
begin
  inc(Run);
  FTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.SemiColonProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while FLine[Run] in [#1..#9, #11, #12, #14..#32] do inc(Run);
end;

procedure TSynVerilogSyn.SquareCloseProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.SquareOpenProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.StarProc;
begin
  inc(Run);
  fTokenID := tkSymbol;
end;

procedure TSynVerilogSyn.StringProc;
begin
  fTokenID := tkString;
  if (FLine[Run + 1] = #34) and (FLine[Run + 2] = #34) then inc(Run, 2);
  repeat
    case FLine[Run] of
      #0, #10, #13: break;
      #92:
        if FLine[Run + 1] = #10 then inc(Run);
    end;
    inc(Run);
  until FLine[Run] = #34;
  if FLine[Run] <> #0 then inc(Run);
end;

procedure TSynVerilogSyn.TildeProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVerilogSyn.AttribProc;
var
  tmp:integer;
  c:char;

begin
  fTokenID := tkSymbol;
  inc(Run);
  tmp:=Run;
  c:=lowercase(fLine[Run]);
  while fLine[tmp] in ['a'..'z','A'..'Z'] do inc(tmp);
  fStringLen:=tmp-fTokenPos;
  fToIdent:=fLine + fTokenPos;
  if (c='b') or
     (c='d') or
     (c='h') or
     keycomp('''include') or
     keycomp('''ifdef') or
     keycomp('''ifndef') or
     keycomp('''define') or
     keycomp('''endif') or
     keycomp('''else') or
     (c='o') then
  begin
    fTokenID := tkAttribute;
    while fLine[Run] in ['a'..'z','A'..'Z'] do inc(Run);
  end;
end;

procedure TSynVerilogSyn.XOrSymbolProc;
begin
  inc(Run);
  fTokenId := tkSymbol;
end;

procedure TSynVerilogSyn.EndTokenProc;
//var tmp:integer;
begin
  fTokenID := tkKey;
  //while fLine[Run]=' ' do inc(Run);
  //tmp:=Run;
  //while Identifiers[fLine[Run]] do inc(Run);
  //fStringLen:=Run-tmp;
  //fToIdent:=fLine + tmp;
  //if not (
  //   keycomp('architecture') or
  //   keycomp('if') or
  //   keycomp('case') or
  //   keycomp('component') or
  //   keycomp('function') or
  //   keycomp('loop') or
  //   keycomp('package') or
  //   keycomp('procedure') or
  //   keycomp('process')
  //   ) then Run:=tmp;
  EndCodeFoldBlock();
end;

procedure TSynVerilogSyn.UnknownProc;
begin
  inc(Run);
  while (fLine[Run] in [#128..#191]) OR // continued utf8 subcode
   ((fLine[Run]<>#0) and (fProcTable[fLine[Run]] = @UnknownProc)) do inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynVerilogSyn.FoldValidProc;
var
  tmp:integer;
  found:boolean;
begin
  tmp:=run;
  found:=false;
  while (not (fLine[tmp] in [#0, #10, #13])) do if fLine[tmp]=';' then
  begin
    found:=true;
    break;
  end else inc(tmp);
  if not found then StartCodeFoldBlock(nil);
end;

procedure TSynVerilogSyn.Next;
begin
  fTokenPos := Run;
  if fRange = rsComment then CommentProc else fProcTable[fLine[Run]];
end;

function TSynVerilogSyn.GetDefaultAttribute(Index: integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result    := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_KEYWORD: Result    := fKeyAttri;
    SYN_ATTR_STRING: Result     := fStringAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result     := fSymbolAttri;
    else
      Result := nil;
  end;
end;

function TSynVerilogSyn.GetEOL: boolean;
begin
  Result := fTokenID = tkNull;
end;

function TSynVerilogSyn.GetToken: string;
var
  Len: longint;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynVerilogSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength:=Run-fTokenPos;
  TokenStart := fLine + fTokenPos;

  //if TokenLength>0 then begin
  //  TokenStart:=@fLine[fTokenPos];
  //end else begin
  //  TokenStart:=nil;
  //end;
end;

function TSynVerilogSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynVerilogSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result    := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKey: Result        := fKeyAttri;
    tkNumber: Result     := fNumberAttri;
    tkSpace: Result      := fSpaceAttri;
    tkString: Result     := fStringAttri;
    tkSymbol: Result     := fSymbolAttri;
    tkAttribute: Result  := fAttribAttri;
    tkIeee: Result       := fIeeeAttri;
    tkSBA:  Result       := fSBAAttri;
    tkUnknown: Result    := fIdentifierAttri;
    else 
      Result := nil;
  end;
end;

function TSynVerilogSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynVerilogSyn.GetTokenPos: integer;
begin
  Result := fTokenPos;
end;

function TSynVerilogSyn.GetRange: Pointer;
begin
//  Result := Pointer(fRange);
  CodeFoldRange.RangeType := Pointer(PtrInt(fRange));
  Result := inherited;
end;

procedure TSynVerilogSyn.ReSetRange;
begin
  inherited;
  fRange := rsUnknown;
end;

function TSynVerilogSyn.GetDrawDivider(Index: integer
  ): TSynDividerDrawConfigSetting;
begin
  Result:=fDivider;
end;

procedure TSynVerilogSyn.SetRange(Value: Pointer);
begin
  //  fRange := TRangeState(Value);
  inherited;
  fRange := TRangeState(PtrUInt(CodeFoldRange.RangeType));
end;

function TSynVerilogSyn.GetIdentChars: TSynIdentChars;
begin
  Result := ['_', '0'..'9', 'a'..'z', 'A'..'Z'];
end;

class function TSynVerilogSyn.GetLanguageName :string;
begin
  Result := SYNS_LangVerilog;
end;

function TSynVerilogSyn.GetSampleSource: string;
begin
  Result :=
   '// ----------------------------------------------------------'+LineEnding+
   '// Module: verilogtest_2'+LineEnding+
   '// ----------------------------------------------------------'+LineEnding+
   ''+LineEnding+
   '`timescale 1ns / 100ps'+LineEnding+
   ''+LineEnding+
   'module nc_test_2(Y, A, B, sel);'+LineEnding+
   '  output [15:0] Y;'+LineEnding+
   '  input [15:0] A, B;'+LineEnding+
   '  input sel;'+LineEnding+
   '  reg [15:0] Y;'+LineEnding+
   '  always @(A or B or sel)'+LineEnding+
   '  if (sel == 1''b0)'+LineEnding+
   '    Y = A;'+LineEnding+
   '  else'+LineEnding+
   '    Y = B;'+LineEnding+
   'endmodule ';
end;

initialization
  MakeIdentTable;
  RegisterPlaceableHighlighter(TSynVerilogSyn);

end.

