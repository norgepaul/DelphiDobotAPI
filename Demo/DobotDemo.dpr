program DobotDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  frmDobotMain in 'frmDobotMain.pas' {frmDobotDemo},
  Dobot.Types in '..\Lib\Dobot.Types.pas',
  Dobot.Dll.Types in '..\Lib\Dobot.Dll.Types.pas',
  Dobot.Dll in '..\Lib\Dobot.Dll.pas',
  Dobot.Controller in '..\Lib\Dobot.Controller.pas',
  Dobot.Classes in '..\Lib\Dobot.Classes.pas',
  Dobot.Utils in '..\Lib\Dobot.Utils.pas',
  Dobot.Script in '..\Lib\Dobot.Script.pas',
  Dobot.Interfaces in '..\Lib\Dobot.Interfaces.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmDobotDemo, frmDobotDemo);
  Application.Run;
end.
