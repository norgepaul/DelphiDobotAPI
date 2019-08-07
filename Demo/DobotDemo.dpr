program DobotDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmDobotMain in 'frmDobotMain.pas' {frmDobotDemo},
  Dobot.Types in '..\Lib\Dobot.Types.pas',
  Dobot.DLL.Types in '..\Lib\Dobot.DLL.Types.pas',
  Dobot.DLL in '..\Lib\Dobot.DLL.pas',
  Dobot.Controller in '..\Lib\Dobot.Controller.pas',
  Dobot.Classes in '..\Lib\Dobot.Classes.pas',
  Dobot.Utils in '..\Lib\Dobot.Utils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDobotDemo, frmDobotDemo);
  Application.Run;
end.
