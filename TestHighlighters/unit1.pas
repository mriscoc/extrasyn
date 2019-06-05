unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  SynEdit, Menus, StdCtrls, ComCtrls, SynEditHighlighter,

  SynHighlighterADSP21xx, SynHighlighterFortran, SynHighlighterFoxpro,  SynHighlighterGalaxy,   SynHighlighterBaan,
  SynHighlighterHaskell,  SynHighlighterCache,  {SynHighlighterDfm,}    SynHighlighterModelica, SynHighlighterCobol,
  {SynHighlighterCPM,}    SynHighlighterCS,      SynHighlighterDml,     SynHighlighterProgress, SynHighlighterEiffel,
  SynHighlighterGWS,     {SynHighlighterHC11,}   SynHighlighterHP48,    SynHighlighterVBScript, SynHighlighterUnreal,
  SynHighlighterVrml97,   SynHighlighterTclTk,   SynHighlighterLDraw,   SynHighlighterRuby,     SynHighlighterInno,
  SynHighlighterAsm,      SynHighlighterDOT,     SynHighlighterIDL,     SynHighlighterKix,     {SynHighlighterMsg,}
  SynHighlighterSDD,      SynHighlighterSml,     SynHighlighterURI,     SynHighlighterM3,       SynHighlighterRC,
  SynHighlighterST,       SynHighlighter8051,    SynHighlighterLua,     SynHighlighterProlog,   SynHighlighterCAC,
  SynHighlighterAWK, SynHighlighterGnuplot;

//   SynHighlighterGeneral;

type

  { TForm1 }
  TForm1 = class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    SynEdit1: TSynEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuClick(Sender: TObject);
  private
    FHighlighters: TFPList;
    procedure SelectHighlighter(AIndex: Integer);
  public

  end;

var
  Form1: TForm1;

implementation

uses
  SynUniHighlighter;

{$R *.lfm}

const
  FORM_CAPTION = 'SyntaxHighlighter Test';

function GetHighlighterCaption(hl: TSynCustomHighlighter): String;
begin
  if (hl is TSynUniSyn) and (TSynUniSyn(hl).Info.General.Name <> '') then
    Result := TSynUniSyn(hl).Info.General.Name
  else
    Result := hl.LanguageName;
end;

function CompareHighlighters(p1, p2: Pointer): Integer;
var
  s1, s2: String;
begin
  s1 := GetHighlighterCaption(TSynCustomHighlighter(p1));
  s2 := GetHighlighterCaption(TSynCustomHighlighter(p2));
  Result := AnsiCompareText(s1, s2);
end;


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);

  procedure SetDefaultColors(const vHighLighter:TSynCustomHighlighter);
  begin
    if Assigned(vHighLighter.CommentAttribute)    then vHighLighter.CommentAttribute.Foreground    := clSilver;
    if Assigned(vHighLighter.IdentifierAttribute) then vHighLighter.IdentifierAttribute.Foreground := clNone;
    if Assigned(vHighLighter.CommentAttribute)    then vHighLighter.CommentAttribute.Foreground    := $00A2A2A2;
    if Assigned(vHighLighter.KeywordAttribute)    then vHighLighter.KeywordAttribute.Foreground    := clNavy;
    //vHighLighter.NumberAttri.ForeGround         := $004080FF;
    if Assigned(vHighLighter.StringAttribute)     then vHighLighter.StringAttribute.ForeGround     := $003FB306;
    if Assigned(vHighLighter.SymbolAttribute)     then vHighLighter.SymbolAttribute.ForeGround     := $00A25151;
  end;

var
  i, c: Integer;
  item: TMenuItem;
  hl: TSynCustomHighlighter;
begin
  Caption := FORM_CAPTION;

  FHighlighters := TFPList.Create;

  // extra highlighters
  FHighlighters.Add(TSynADSP21xxSyn.Create(self));
  FHighlighters.Add(TSynFortranSyn.Create(self));
  FHighlighters.Add(TSynFoxproSyn.Create(self));
  FHighlighters.Add(TSynGalaxySyn.Create(self));
  FHighlighters.Add(TSynBaanSyn.Create(self));
  FHighlighters.Add(TSynAWKSyn.Create(self));
  FHighlighters.Add(TSynHaskellSyn.Create(self));
  FHighlighters.Add(TSynCacheSyn.Create(self));
  FHighlighters.Add(TSynModelicaSyn.Create(self));
  FHighlighters.Add(TSynCobolSyn.Create(self));
  FHighlighters.Add(TSynCSSyn.Create(self));
  FHighlighters.Add(TSynDmlSyn.Create(self));
  FHighlighters.Add(TSynProgressSyn.Create(self));
  FHighlighters.Add(TSynEiffelSyn.Create(self));
  FHighlighters.Add(TSynGWScriptSyn.Create(self));
  FHighlighters.Add(TSynHP48Syn.Create(self));
  FHighlighters.Add(TSynVBScriptSyn.Create(self));
  FHighlighters.Add(TSynUnrealSyn.Create(self));
  FHighlighters.Add(TSynVrml97Syn.Create(self));
  FHighlighters.Add(TSynTclTkSyn.Create(self));
  FHighlighters.Add(TSynLDRSyn.Create(self));
  FHighlighters.Add(TSynRubySyn.Create(self));
  FHighlighters.Add(TSynInnoSyn.Create(self));
  FHighlighters.Add(TSynAsmSyn.Create(self));
  FHighlighters.Add(TSynDOTSyn.Create(self));
  FHighlighters.Add(TSynIdlSyn.Create(self));
  FHighlighters.Add(TSynKixSyn.Create(self));
  FHighlighters.Add(TSynSDDSyn.Create(self));
  FHighlighters.Add(TSynSMLSyn.Create(self));
  FHighlighters.Add(TSynURISyn.Create(self));
  FHighlighters.Add(TSynM3Syn.Create(self));
  FHighlighters.Add(TSynRCSyn.Create(self));
  FHighlighters.Add(TSynPrologSyn.Create(Self));
  FHighlighters.Add(TSynLuaSyn.Create(Self));
  FHighlighters.Add(TSyn8051Syn.Create(Self));
  FHighlighters.Add(TSynCACSyn.Create(Self));
  FHighlighters.Add(TSynSTSyn.Create(self));
  FHighlighters.Add(TSynGnuplotSyn.Create(self));

//  FHighlighters.Add(TSynCPMSyn.Create(self));
//  FHighlighters.Add(TSynGeneralSyn.Create(self));
//  FHighlighters.Add(TSynDfmSyn.Create(self));
//  FHighlighters.Add/TSynUniSyn.Create(self));
//  FHighlighters.Add(TSynMsgSyn.Create(self));

  FHighlighters.Sort(@CompareHighlighters);

  for i:=0 to FHighlighters.Count-1 do begin
    hl := TSynCustomHighlighter(FHighlighters[i]);
    if Assigned(hl) then begin
      SetDefaultColors(hl);
      item := TMenuItem.Create(self);
      item.Tag := i+1; //0 = unknown highlighter
      try
        item.Caption := Format('%d - %s', [i, GetHighlighterCaption(hl)]);
      except
        on E : Exception  do
          ShowMessage(E.Message+LineEnding+' at index '+inttostr(i));
      end;
//      item.Hint := hl.ClassName;
      item.OnClick := @MenuClick;
      MenuItem1.Add(item);
    end;
  end;

  SelectHighlighter(12);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FHighlighters.Free;
end;

procedure TForm1.MenuClick(Sender: TObject);
var
  i, idx, c:Integer;
begin
  c := ComponentCount-1;
  if (Sender is TMenuItem) and (TMenuItem(Sender).Tag > 0) then begin
    idx := TMenuItem(Sender).Tag - 1;
    SelectHighlighter(idx);
  end;
end;

procedure TForm1.SelectHighlighter(AIndex: Integer);
begin
  SynEdit1.Highlighter := TSynCustomHighlighter(FHighlighters[AIndex]);
  SynEdit1.Text := SynEdit1.Highlighter.SampleSource;
  Caption := Format('%s - %s [%s]', [
    FORM_CAPTION, SynEdit1.Highlighter.LanguageName, SynEdit1.Highlighter.ClassName
  ]);
end;

end.




