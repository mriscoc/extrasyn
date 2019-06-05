unit uHighlighterReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  SynHighlighter8051,
  SynHighlighterADSP21xx,
  SynHighlighterAsm,
  SynHighlighterAWK,
  SynHighlighterBaan,
  SynHighlighterCAC,
  SynHighlighterCache,
  SynHighlighterCobol,
//  SynHighlighterCPM,
  SynHighlighterCS,
  SynHighlighterDml,
  SynHighlighterDOT,
  SynHighlighterEiffel,
  SynHighlighterFortran,
  SynHighlighterFoxpro,
  SynHighlighterGalaxy,
  SynHighlighterGnuPlot,
  SynHighlighterGWS,
  SynHighlighterHaskell,
  SynHighlighterHP48,
  SynHighlighterIDL,
  SynHighlighterInno,
  SynHighlighterKix,
  SynHighlighterLDraw,
  SynHighlighterLua,
  SynHighlighterM3,
  SynHighlighterModelica,
  SynHighlighterProgress,
  SynHighlighterProlog,
  SynHighlighterRC,
  SynHighlighterRuby,
  SynHighlighterSDD,
  SynHighlighterSml,
  SynHighlighterTclTk,
  SynHighlighterUnreal,
  SynHighlighterVBScript,
  SynHighlighterVrml97;

procedure Register;

implementation

uses
  LResources;

{$R images.res}

procedure Register;
begin
  RegisterComponents('SynEdit Highlighters',[
    TSyn8051Syn,
    TSynADSP21xxSyn,
    TSynAsmSyn,
    TSynAWKSyn,
    TSynBaanSyn,
    TSynCacheSyn,
    TSynCACSyn,
    TSynCobolSyn,
//    TSynCPMSyn,
    TSynCSSyn,
    TSynDmlSyn,
    TSynDOTSyn,
    TSynEiffelSyn,
    TSynFortranSyn,
    TSynFoxproSyn,
    TSynGalaxySyn,
    TSynGnuplotSyn,
    TSynGWScriptSyn,
    TSynHaskellSyn,
    TSynHP48Syn,
    TSynIdlSyn,
    TSynInnoSyn,
    TSynKixSyn,
    TSynLDRSyn,
    TSynLuaSyn,
    TSynM3Syn,
    TSynModelicaSyn,
    TSynProgressSyn,
    TSynPrologSyn,
    TSynRCSyn,
    TSynRubySyn,
    TSynSMLSyn,
    TSynSDDSyn,
    TSynTclTkSyn,
    TSynUnrealSyn,
    TSynVBScriptSyn,
    TSynVrml97Syn
  ]);
end;

end.

