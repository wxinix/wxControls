unit wxControlsReg;

interface

uses
  System.Classes, wxObjectInspector.Core;

procedure Register;

implementation

resourcestring
  SzControls = 'wxObjectInspector';

procedure Register;
begin
  RegisterComponents(SzControls, [TzObjectInspector]);
end;

end.
