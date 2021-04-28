program InspDemo;

uses
  FastMM5,
  Vcl.Forms,
  uMain in 'uMain.pas' {Main},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
