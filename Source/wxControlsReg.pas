unit wxControlsReg;

interface

uses
  System.Classes, wxObjectInspector.Core;

procedure Register;

implementation

resourcestring
  SwxControls = 'wxObjectInspector';

procedure Register;
begin
  RegisterComponents(SwxControls, [TwxObjectInspector]);
end;

end.
