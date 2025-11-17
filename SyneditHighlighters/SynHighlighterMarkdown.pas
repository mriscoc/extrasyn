{-------------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
https://www.mozilla.org/MPL/

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is SynHighlighterMarkdown.pas, released 2025-09-29.

The Initial Developer of the Original Code is Miguel A. Risco-Castillo.
All Rights Reserved.

Alternatively, the contents of this file may be used under the terms of the
GNU General Public License Version 2 or later (the  "GPL"), in which case
the provisions of GPL are applicable instead of those above. If you wish to
allow use of your version of this file only under the terms of the GPL and
not to allow others to use your version of this file under the MPL, indicate
your decision by deleting the provisions above and replace them with the
notice and other provisions required by the GPL. If you do not delete the
provisions above, a recipient may use your version of this file under either
the MPL or the GPL.
-------------------------------------------------------------------------------}
{
@abstract(Provides a Markdown highlighter for SynEdit)
@author(Miguel A. Risco-Castillo)
@created(2025-09-29)
The SynHighlighterMarkdown unit provides SynEdit with a Markdown highlighter.
}

unit SynHighlighterMarkdown;

{$I extrasyn.inc}

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  Classes;

type
  TtkTokenKind = (
    tkText,
    tkHeader,
    tkBold,
    tkItalic,
    tkCode,
    tkCodeBlock,
    tkBlockQuote,
    tkHorizontalRule,
    tkListItem,
    tkLink,
    tkImage,
    tkSpace,
    tkSymbol,
    tkComment,
    tkNull
  );

  TRangeState = (rsText, rsCodeBlock);

  TProcTableProc = procedure of object;

type
  TSynMarkdownSyn = class(TSynCustomHighlighter)
  private
    fLine: PChar;
    fLineNumber: Integer;
    fProcTable: array[#0..#255] of TProcTableProc;
    Run: LongInt;
    fTokenPos: Integer;
    fTokenID: TtkTokenKind;
    fRange: TRangeState;
    fTextAttri: TSynHighlighterAttributes;
    fHeaderAttri: TSynHighlighterAttributes;
    fBoldAttri: TSynHighlighterAttributes;
    fItalicAttri: TSynHighlighterAttributes;
    fCodeAttri: TSynHighlighterAttributes;
    fCodeBlockAttri: TSynHighlighterAttributes;
    fBlockQuoteAttri: TSynHighlighterAttributes;
    fHorizontalRuleAttri: TSynHighlighterAttributes;
    fListItemAttri: TSynHighlighterAttributes;
    fLinkAttri: TSynHighlighterAttributes;
    fImageAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fCommentAttri: TSynHighlighterAttributes;

    procedure MakeMethodTables;
    procedure AtSignProc;
    procedure BacktickProc;
    procedure BraceOpenProc;
    procedure BraceCloseProc;
    procedure BracketOpenProc;
    procedure BracketCloseProc;
    procedure StarProc;
    procedure NumberSignProc;
    procedure PlusProc;
    procedure MinusProc;
    procedure DotProc;
    procedure GreaterProc;
    procedure ExclamationProc;
    procedure UnderscoreProc;
    procedure TildeProc;
    procedure PipeProc;
    procedure LessThanProc;
    procedure CRProc;
    procedure LFProc;
    procedure NullProc;
    procedure SpaceProc;
    procedure IdentProc;
    procedure CodeBlockProc;
    procedure CommentProc;
    procedure LinkImageProc(ImageFlag: Boolean);
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    class function GetLanguageName: string; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
      override;
    function GetEol: Boolean; override;
    function GetRange: Pointer; override;
    function GetTokenID: TtkTokenKind;
    procedure SetLine(const NewValue: String; LineNumber: Integer); override;
    function GetToken: String; override;
    procedure GetTokenEx(out TokenStart: PChar; out TokenLength: integer); override;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: integer; override;
    function GetTokenPos: Integer; override;
    procedure Next; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
  published
    property TextAttri: TSynHighlighterAttributes read fTextAttri write fTextAttri;
    property HeaderAttri: TSynHighlighterAttributes read fHeaderAttri write fHeaderAttri;
    property BoldAttri: TSynHighlighterAttributes read fBoldAttri write fBoldAttri;
    property ItalicAttri: TSynHighlighterAttributes read fItalicAttri write fItalicAttri;
    property CodeAttri: TSynHighlighterAttributes read fCodeAttri write fCodeAttri;
    property CodeBlockAttri: TSynHighlighterAttributes read fCodeBlockAttri write fCodeBlockAttri;
    property BlockQuoteAttri: TSynHighlighterAttributes read fBlockQuoteAttri write fBlockQuoteAttri;
    property HorizontalRuleAttri: TSynHighlighterAttributes read fHorizontalRuleAttri write fHorizontalRuleAttri;
    property ListItemAttri: TSynHighlighterAttributes read fListItemAttri write fListItemAttri;
    property LinkAttri: TSynHighlighterAttributes read fLinkAttri write fLinkAttri;
    property ImageAttri: TSynHighlighterAttributes read fImageAttri write fImageAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
  end;

implementation

uses
  SynEditStrConst, SynEditStrConstExtra;

procedure TSynMarkdownSyn.MakeMethodTables;
var
  I: Char;
begin
  for I := #0 to #255 do
    case I of
      #0  : fProcTable[I] := @NullProc;
      #10 : fProcTable[I] := @LFProc;
      #13 : fProcTable[I] := @CRProc;
      '@' : fProcTable[I] := @AtSignProc;
      '`' : fProcTable[I] := @BacktickProc;
      '{' : fProcTable[I] := @BraceOpenProc;
      '}' : fProcTable[I] := @BraceCloseProc;
      '[' : fProcTable[I] := @BracketOpenProc;
      ']' : fProcTable[I] := @BracketCloseProc;
      '*' : fProcTable[I] := @StarProc;
      '#' : fProcTable[I] := @NumberSignProc;
      '+' : fProcTable[I] := @PlusProc;
      '-' : fProcTable[I] := @MinusProc;
      '.' : fProcTable[I] := @DotProc;
      '>' : fProcTable[I] := @GreaterProc;
      '!' : fProcTable[I] := @ExclamationProc;
      '_' : fProcTable[I] := @UnderscoreProc;
      '~' : fProcTable[I] := @TildeProc;
      '|' : fProcTable[I] := @PipeProc;
      '<' : fProcTable[I] := @LessThanProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[I] := @SpaceProc;
      else
        fProcTable[I] := @IdentProc;
    end;
end;

constructor TSynMarkdownSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fTextAttri := TSynHighlighterAttributes.Create(SYNS_AttrText);
  AddAttribute(fTextAttri);

  fHeaderAttri := TSynHighlighterAttributes.Create('Header');
  fHeaderAttri.Style := [fsBold];
  fHeaderAttri.Foreground := clNavy;
  AddAttribute(fHeaderAttri);

  fBoldAttri := TSynHighlighterAttributes.Create('Bold');
  fBoldAttri.Style := [fsBold];
  AddAttribute(fBoldAttri);

  fItalicAttri := TSynHighlighterAttributes.Create('Italic');
  fItalicAttri.Style := [fsItalic];
  AddAttribute(fItalicAttri);

  fCodeAttri := TSynHighlighterAttributes.Create('Code');
  fCodeAttri.Foreground := clMaroon;
  AddAttribute(fCodeAttri);

  fCodeBlockAttri := TSynHighlighterAttributes.Create('CodeBlock');
  fCodeBlockAttri.Foreground := clTeal;
  AddAttribute(fCodeBlockAttri);

  fBlockQuoteAttri := TSynHighlighterAttributes.Create('BlockQuote');
  fBlockQuoteAttri.Foreground := clGray;
  fBlockQuoteAttri.Style := [fsItalic];
  AddAttribute(fBlockQuoteAttri);

  fHorizontalRuleAttri := TSynHighlighterAttributes.Create('HorizontalRule');
  fHorizontalRuleAttri.Foreground := clGray;
  AddAttribute(fHorizontalRuleAttri);

  fListItemAttri := TSynHighlighterAttributes.Create('ListItem');
  fListItemAttri.Foreground := clFuchsia;
  AddAttribute(fListItemAttri);

  fLinkAttri := TSynHighlighterAttributes.Create('Link');
  fLinkAttri.Foreground := clBlue;
  fLinkAttri.Style := [fsUnderline];
  AddAttribute(fLinkAttri);

  fImageAttri := TSynHighlighterAttributes.Create('Image');
  fImageAttri.Foreground := clGreen;
  AddAttribute(fImageAttri);

  fSpaceAttri := TSynHighlighterAttributes.Create(SYNS_AttrSpace);
  AddAttribute(fSpaceAttri);

  fSymbolAttri := TSynHighlighterAttributes.Create(SYNS_AttrSymbol);
  AddAttribute(fSymbolAttri);

  fCommentAttri := TSynHighlighterAttributes.Create(SYNS_AttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGray;
  AddAttribute(fCommentAttri);

  MakeMethodTables;
  SetAttributesOnChange(@DefHighlightChange);
  fDefaultFilter := SYNS_FilterMarkdown;
  fRange := rsText;
end;

function TSynMarkdownSyn.GetDefaultAttribute(Index: integer): TSynHighlighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fTextAttri;
    SYN_ATTR_KEYWORD: Result := fHeaderAttri;
    SYN_ATTR_STRING: Result := fTextAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynMarkdownSyn.GetEol: Boolean;
begin
  Result := fTokenId = tkNull;
end;

function TSynMarkdownSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

function TSynMarkdownSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynMarkdownSyn.GetTokenAttribute: TSynHighlighterAttributes;
begin
  if fRange = rsCodeBlock then
     Result := fCodeBlockAttri
  else case fTokenID of
    tkText: Result := fTextAttri;
    tkHeader: Result := fHeaderAttri;
    tkBold: Result := fBoldAttri;
    tkItalic: Result := fItalicAttri;
    tkCode: Result := fCodeAttri;
    tkCodeBlock: Result := fCodeBlockAttri;
    tkBlockQuote: Result := fBlockQuoteAttri;
    tkHorizontalRule: Result := fHorizontalRuleAttri;
    tkListItem: Result := fListItemAttri;
    tkLink: Result := fLinkAttri;
    tkImage: Result := fImageAttri;
    tkSpace: Result := fSpaceAttri;
    tkSymbol: Result := fSymbolAttri;
    tkComment: Result := fCommentAttri;
    else Result := fTextAttri;
  end;
end;

function TSynMarkdownSyn.GetTokenKind: integer;
begin
  Result := Ord(fTokenId);
end;

function TSynMarkdownSyn.GetTokenPos: Integer;
begin
  Result := fTokenPos;
end;

procedure TSynMarkdownSyn.Next;
begin
  fTokenPos := Run;
  fProcTable[fLine[Run]]();
end;

procedure TSynMarkdownSyn.AtSignProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynMarkdownSyn.BacktickProc;
begin
  // Check for code block
  if (fLine[Run+1] = '`') and (fLine[Run+2] = '`') then
  begin
    CodeBlockProc;
  end
  else // Handle inline code
  begin
    fTokenID := tkCode;
    inc(Run);
    while not (fLine[Run] in [#0, #10, #13, '`']) do
      inc(Run);
    if fLine[Run] = '`' then
      inc(Run);
  end;
end;

procedure TSynMarkdownSyn.CodeBlockProc;
begin
  fTokenID := tkCodeBlock;
  inc(Run, 3);
  if fRange = rsCodeBlock then
    fRange := rsText
  else
    fRange := rsCodeBlock;

  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if (fLine[Run] = '`') and (fLine[Run+1] = '`') and (fLine[Run+2] = '`') then
    begin
      Inc(Run, 3);
      Exit;
    end;
    Inc(Run);
  end;
end;


procedure TSynMarkdownSyn.BraceOpenProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynMarkdownSyn.BraceCloseProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynMarkdownSyn.BracketOpenProc;
begin
  // Check if it's part of a link or just a bracket
  if fLine[Run+1] <> #0 then
  begin
    LinkImageProc(False);
  end
  else
  begin
    fTokenID := tkSymbol;
    inc(Run);
  end;
end;

procedure TSynMarkdownSyn.BracketCloseProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynMarkdownSyn.StarProc;
var
  Count: Integer;
begin
  // Count consecutive stars
  Count := 1;
  while fLine[Run+Count] = '*' do
    Inc(Count);

  if Count >= 4 then
  begin
    // Treat 4 or more asterisk as symbols
    fTokenID := tkSymbol;
    Inc(Run, Count);  // Include all underscores in the token
  end
  else if Count = 3 then
  begin
    // Bold+Italic
    fTokenID := tkBold;
    inc(Run, 3);
    while not (fLine[Run] in [#0, #10, #13]) and
          not ((fLine[Run] = '*') and (fLine[Run+1] = '*') and (fLine[Run+2] = '*')) do
      inc(Run);
    if (fLine[Run] = '*') and (fLine[Run+1] = '*') and (fLine[Run+2] = '*') then
      inc(Run, 3);
  end
  else if Count = 2 then
  begin
    // Bold
    fTokenID := tkBold;
    inc(Run, 2);
    while not (fLine[Run] in [#0, #10, #13]) and
          not ((fLine[Run] = '*') and (fLine[Run+1] = '*')) do
      inc(Run);
    if (fLine[Run] = '*') and (fLine[Run+1] = '*') then
      inc(Run, 2);
  end
  else if Count = 1 then
  begin
    // Italic or list item
    if (fLine[Run+1] = #32) then
    begin
      // List item
      fTokenID := tkListItem;
      inc(Run);
      if fLine[Run] = ' ' then inc(Run);
    end
    else
    begin
      // Italic
      fTokenID := tkItalic;
      inc(Run);
      while not (fLine[Run] in [#0, #10, #13, '*']) do
        inc(Run);
      if fLine[Run] = '*' then inc(Run);
    end;
  end;
end;

procedure TSynMarkdownSyn.NumberSignProc;
var
  Count: Integer;
  IsHeader: Boolean;
begin
  // Check if it's a header (# at start of line, followed by space)
  Count := 0;
  IsHeader := (Run = 0);

  if IsHeader then
  begin
    // Count the number of consecutive # symbols
    while fLine[Run+Count] = '#' do
      Inc(Count);

    // Must be followed by space to be a header
    if (Count > 0) and (fLine[Run+Count] = ' ') then
    begin
      fTokenID := tkHeader;
      // Skip to the end of line to capture the entire header
      while not (fLine[Run] in [#0, #10, #13]) do
        inc(Run);
    end
    else
    begin
      fTokenID := tkSymbol;
      inc(Run);
    end;
  end
  else
  begin
    fTokenID := tkSymbol;
    inc(Run);
  end;
end;

procedure TSynMarkdownSyn.PlusProc;
begin
  // Check if it's a list item
  if (Run = 0) or (fLine[Run-1] in [#9, #32]) then
  begin
    fTokenID := tkListItem;
    inc(Run);
    if fLine[Run] = ' ' then inc(Run);
  end
  else
  begin
    fTokenID := tkSymbol;
    inc(Run);
  end;
end;

procedure TSynMarkdownSyn.MinusProc;
var
  Count: Integer;
  IsHR: Boolean;
begin
  // Check for horizontal rule (---)
  if Run = 0 then
  begin
    Count := 0;
    while fLine[Run+Count] = '-' do
      Inc(Count);

    IsHR := (Count >= 3) and (fLine[Run+Count] in [#0, #10, #13, #32]);

    if IsHR then
    begin
      fTokenID := tkHorizontalRule;
      Inc(Run, Count);
    end
    else if fLine[Run+1] = ' ' then // List item
    begin
      fTokenID := tkListItem;
      inc(Run);
      if fLine[Run] = ' ' then inc(Run);
    end
    else
    begin
      fTokenID := tkSymbol;
      inc(Run);
    end;
  end
  else
  begin
    fTokenID := tkSymbol;
    inc(Run);
  end;
end;

procedure TSynMarkdownSyn.DotProc;
begin
  fTokenID := tkSymbol;
  inc(Run);
end;

procedure TSynMarkdownSyn.GreaterProc;
begin
  // Check if it's a blockquote
  if Run = 0 then
  begin
    fTokenID := tkBlockQuote;
    // Skip to the end of line to capture the entire blockquote
    while not (fLine[Run] in [#0, #10, #13]) do
      inc(Run);
  end
  else
  begin
    fTokenID := tkSymbol;
    inc(Run);
  end;
end;

procedure TSynMarkdownSyn.ExclamationProc;
begin
  // Check if it's an image ![alt](url)
  if fLine[Run+1] = '[' then
  begin
    LinkImageProc(True);
  end
  else
  begin
    fTokenID := tkSymbol;
    inc(Run);
  end;
end;

procedure TSynMarkdownSyn.UnderscoreProc;
var
  Count: Integer;
begin
  // Count consecutive underscores
  Count := 1;
  while fLine[Run+Count] = '_' do
    Inc(Count);

  if Count >= 4 then
  begin
    // Treat 4 or more underscores as symbols
    fTokenID := tkSymbol;
    Inc(Run, Count);  // Include all underscores in the token
  end
  else if Count = 3 then
  begin
    // Bold+Italic
    fTokenID := tkBold;
    inc(Run, 3);
    while not (fLine[Run] in [#0, #10, #13]) and
          not ((fLine[Run] = '_') and (fLine[Run+1] = '_') and (fLine[Run+2] = '_')) do
      inc(Run);
    if (fLine[Run] = '_') and (fLine[Run+1] = '_') and (fLine[Run+2] = '_') then
      inc(Run, 3);
  end
  else if Count = 2 then
  begin
    // Bold
    fTokenID := tkBold;
    inc(Run, 2);
    while not (fLine[Run] in [#0, #10, #13]) and
          not ((fLine[Run] = '_') and (fLine[Run+1] = '_')) do
      inc(Run);
    if (fLine[Run] = '_') and (fLine[Run+1] = '_') then
      inc(Run, 2);
  end
  else if Count = 1 then
  begin
    // Italic
    fTokenID := tkItalic;
    inc(Run);
    while not (fLine[Run] in [#0, #10, #13, '_']) do
      inc(Run);
    if fLine[Run] = '_' then inc(Run);
  end;
end;

procedure TSynMarkdownSyn.TildeProc;
begin
  // Check for strikethrough (~~text~~)
  if fLine[Run+1] = '~' then
  begin
    // We don't have a specific attribute for strikethrough, using bold for now
    fTokenID := tkBold;
    inc(Run, 2);
    while not (fLine[Run] in [#0, #10, #13]) and
          not ((fLine[Run] = '~') and (fLine[Run+1] = '~')) do
      inc(Run);
    if (fLine[Run] = '~') and (fLine[Run+1] = '~') then
      inc(Run, 2);
  end
  else
  begin
    fTokenID := tkSymbol;
    inc(Run);
  end;
end;

procedure TSynMarkdownSyn.PipeProc;
begin
  // Check for table row
  if (Run = 0) or (fLine[Run-1] in [#9, #32]) then
  begin
    fTokenID := tkSymbol;
    // Tables are highlighted as symbols for now
    while not (fLine[Run] in [#0, #10, #13]) do
      inc(Run);
  end
  else
  begin
    fTokenID := tkSymbol;
    inc(Run);
  end;
end;

procedure TSynMarkdownSyn.LessThanProc;
begin
  // Check for HTML comment
  if (fLine[Run+1] = '!') and (fLine[Run+2] = '-') and (fLine[Run+3] = '-') then
  begin
    CommentProc;
  end
  else
  begin
    fTokenID := tkSymbol;
    inc(Run);
  end;
end;

procedure TSynMarkdownSyn.CRProc;
begin
  fTokenID := tkSpace;
  Inc(Run);
  if fLine[Run] = #10 then Inc(Run);
end;

procedure TSynMarkdownSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynMarkdownSyn.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TSynMarkdownSyn.SpaceProc;
begin
  fTokenID := tkSpace;
  repeat
    Inc(Run);
  until (fLine[Run] > #32) or (fLine[Run] in [#0, #10, #13]);
end;

procedure TSynMarkdownSyn.IdentProc;
begin
  fTokenID := tkText;

  // Check if we're already at a special character
  if fLine[Run] in [#0, #10, #13, #32, '*', '_', '`', '#', '[', '!', '<', '>', '~', '|', '@'] then
    inc(Run)  // Must advance at least one character
  else
  begin
    repeat
      Inc(Run);
    until fLine[Run] in [#0, #10, #13, #32, '*', '_', '`', '#', '[', '!', '<', '>', '~', '|', '@'];
  end;
end;

procedure TSynMarkdownSyn.CommentProc;
begin
  fTokenID := tkComment;
  inc(Run, 4); // Skip <!--

  while not (fLine[Run] in [#0, #10, #13]) do
  begin
    if (fLine[Run] = '-') and (fLine[Run+1] = '-') and (fLine[Run+2] = '>') then
    begin
      Inc(Run, 3);
      Exit;
    end;
    Inc(Run);
  end;
end;

procedure TSynMarkdownSyn.LinkImageProc(ImageFlag: Boolean);
var
  LinkStart, TextEnd, UrlEnd: Integer;
  HasUrl: Boolean;
begin
  LinkStart := Run;
  HasUrl := False;

  if ImageFlag then
  begin
    // Skip ![
    Inc(Run, 2);
    fTokenID := tkImage;
  end
  else
  begin
    // Skip [
    Inc(Run);
    fTokenID := tkLink;
  end;

  // Find closing bracket
  TextEnd := Run;
  while not (fLine[TextEnd] in [#0, #10, #13, ']']) do
    Inc(TextEnd);

  if fLine[TextEnd] = ']' then
  begin
    // Check if URL part exists (...)
    if (fLine[TextEnd+1] = '(') then
    begin
      HasUrl := True;
      UrlEnd := TextEnd + 2;
      // Find closing parenthesis
      while not (fLine[UrlEnd] in [#0, #10, #13, ')']) do
        Inc(UrlEnd);

      if fLine[UrlEnd] = ')' then
        Run := UrlEnd + 1
      else
        Run := TextEnd + 1;
    end
    else
    begin
      Run := TextEnd + 1;
    end;
  end
  else
  begin
    Run := TextEnd;
  end;
end;

procedure TSynMarkdownSyn.SetLine(const NewValue: String; LineNumber: Integer);
begin
  fLine := PChar(NewValue);
  Run := 0;
  fLineNumber := LineNumber;
  Next;
end;

function TSynMarkdownSyn.GetToken: String;
var
  Len: LongInt;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FLine + fTokenPos), Len);
end;

procedure TSynMarkdownSyn.GetTokenEx(out TokenStart: PChar; out TokenLength: integer);
begin
  TokenLength := Run - fTokenPos;
  TokenStart := FLine + fTokenPos;
end;

procedure TSynMarkdownSyn.ResetRange;
begin
  fRange := rsText;
end;

procedure TSynMarkdownSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynMarkdownSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterMarkdown;
end;

class function TSynMarkdownSyn.GetLanguageName: string;
begin
  Result := SYNS_LangMarkdown;
end;

function TSynMarkdownSyn.GetSampleSource: string;
begin
  Result :=
    '# Markdown Sample' + LineEnding +
    '' + LineEnding +
    'This is a **bold** and *italic* text example. Also ~~strikethrough~~.' + LineEnding +
    '' + LineEnding +
    '## Lists' + LineEnding +
    '' + LineEnding +
    '- Item 1' + LineEnding +
    '- Item 2' + LineEnding +
    '  - Nested item' + LineEnding +
    '' + LineEnding +
    '1. Ordered item 1' + LineEnding +
    '2. Ordered item 2' + LineEnding +
    '' + LineEnding +
    '## Code' + LineEnding +
    '' + LineEnding +
    '```pascal' + LineEnding +
    'procedure TForm1.Button1Click(Sender: TObject);' + LineEnding +
    'begin' + LineEnding +
    '  ShowMessage(''Hello World!'');' + LineEnding +
    'end;' + LineEnding +
    '```' + LineEnding +
    '' + LineEnding +
    'Inline `code` example.' + LineEnding +
    '' + LineEnding +
    '## Links and Images' + LineEnding +
    '' + LineEnding +
    '[Visit GitHub](https://github.com)' + LineEnding +
    '' + LineEnding +
    '![Image Example](image.jpg)' + LineEnding +
    '' + LineEnding +
    '> This is a blockquote.' + LineEnding +
    '> It can span multiple lines.' + LineEnding +
    '' + LineEnding +
    '---' + LineEnding +
    '' + LineEnding +
    '| Header 1 | Header 2 |' + LineEnding +
    '| -------- | -------- |' + LineEnding +
    '| Cell 1   | Cell 2   |' + LineEnding +
    '' + LineEnding +
    '<!-- This is a comment -->';
end;

initialization
  RegisterPlaceableHighlighter(TSynMarkdownSyn);
end.
