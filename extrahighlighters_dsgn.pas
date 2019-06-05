{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit ExtraHighlighters_Dsgn;

{$warn 5023 off : no warning about unused units}
interface

uses
  uHighlighterReg, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('uHighlighterReg', @uHighlighterReg.Register);
end;

initialization
  RegisterPackage('ExtraHighlighters_Dsgn', @Register);
end.
