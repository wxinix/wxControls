program InspDemo;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Main};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
