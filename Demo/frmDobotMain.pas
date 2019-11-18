unit frmDobotMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,

  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, System.Actions,
  FMX.ActnList, FMX.Controls.Presentation, FMX.Layouts, FMX.Edit,
  FMX.ListBox, FMX.ScrollBox, FMX.Memo, FMX.TabControl,

  Dobot.Controller,
  Dobot.Script,
  Dobot.Classes,
  Dobot.Types,
  Dobot.Utils,
  Dobot.Dll.Types;

type
  TfrmDobotDemo = class(TForm)
    Layout3: TLayout;
    Button1: TButton;
    ActionList1: TActionList;
    Button2: TButton;
    actHome: TAction;
    actMoveLinear: TAction;
    actMoveJoint: TAction;
    actGripperOff: TAction;
    actGripperOpen: TAction;
    actGripperClose: TAction;
    actSetPoint: TAction;
    actMoveToPoint: TAction;
    actConnect: TAction;
    actDisconnect: TAction;
    tmrDobot: TTimer;
    layMain: TLayout;
    actStop: TAction;
    edtSerialPort: TEdit;
    TabControl1: TTabControl;
    tabManual: TTabItem;
    tabScript: TTabItem;
    Layout1: TLayout;
    GroupBox1: TGroupBox;
    Button3: TButton;
    Button25: TButton;
    GroupBox2: TGroupBox;
    Layout6: TLayout;
    cbPTPMode: TComboBox;
    edtPTPX: TEdit;
    edtPTPR: TEdit;
    edtPTPZ: TEdit;
    edtPTPY: TEdit;
    Layout7: TLayout;
    Button4: TButton;
    Button24: TButton;
    Layout8: TLayout;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    GroupBox3: TGroupBox;
    TabControl2: TTabControl;
    tabGripper: TTabItem;
    Button5: TButton;
    Button8: TButton;
    Button9: TButton;
    Sucker: TTabItem;
    Laser: TTabItem;
    GroupBox4: TGroupBox;
    Layout5: TLayout;
    lblJoint1: TLabel;
    lblJoint4: TLabel;
    lblJoint3: TLabel;
    lblJoint2: TLabel;
    Button6: TButton;
    Button17: TButton;
    Button18: TButton;
    Button19: TButton;
    Button20: TButton;
    Button21: TButton;
    Button22: TButton;
    Button23: TButton;
    GroupBox5: TGroupBox;
    Layout4: TLayout;
    lblPosX: TLabel;
    lblPosServo: TLabel;
    lblPosZ: TLabel;
    lblPosY: TLabel;
    Button7: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button15: TButton;
    Button16: TButton;
    Layout2: TLayout;
    Layout9: TLayout;
    Label9: TLabel;
    memAlarms: TMemo;
    Layout10: TLayout;
    Label10: TLabel;
    memLog: TMemo;
    Splitter1: TSplitter;
    actScriptStart: TAction;
    actScriptStop: TAction;
    memScript: TMemo;
    lblMoving: TLabel;
    GroupBox6: TGroupBox;
    Layout11: TLayout;
    cbScriptPTPMode: TComboBox;
    edtScriptX: TEdit;
    edtScriptR: TEdit;
    edtScriptZ: TEdit;
    edtScriptY: TEdit;
    Layout12: TLayout;
    Layout13: TLayout;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button27: TButton;
    actScriptAddMove: TAction;
    actScriptHome: TAction;
    Button26: TButton;
    actScriptSessionHome: TAction;
    actScriptWaitUntilIdle: TAction;
    Button29: TButton;
    Button30: TButton;
    Button28: TButton;
    actScriptExecQueue: TAction;
    Button31: TButton;
    Button32: TButton;
    procedure actDisconnectExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actHomeExecute(Sender: TObject);
    procedure actGripperOffExecute(Sender: TObject);
    procedure actGripperOpenExecute(Sender: TObject);
    procedure actGripperCloseExecute(Sender: TObject);
    procedure actSetPointExecute(Sender: TObject);
    procedure actMoveToPointExecute(Sender: TObject);
    procedure tmrDobotTimer(Sender: TObject);
    procedure OnMoveLinearClick(Sender: TObject);
    procedure OnMoveJointClick(Sender: TObject);
    procedure actStopExecute(Sender: TObject);
    procedure actScriptAddMoveExecute(Sender: TObject);
    procedure ActionList1Update(Action: TBasicAction; var Handled: Boolean);
    procedure actScriptHomeExecute(Sender: TObject);
    procedure actScriptSessionHomeExecute(Sender: TObject);
    procedure actScriptWaitUntilIdleExecute(Sender: TObject);
    procedure actScriptExecQueueExecute(Sender: TObject);
    procedure actScriptStartExecute(Sender: TObject);
    procedure actScriptStopExecute(Sender: TObject);
  private
    FDobotController: TDobotScript;

    FPose: TPose;
    FDobotAlarms: TStringList;
    procedure UpdateDobotAlarms;
    procedure UpdateDobotPose;
    procedure OnLog(Sender: TObject; const LogText: String; const LogLevel: TDobotLogLevel);
    procedure AddScriptLine(const Text: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmDobotDemo: TfrmDobotDemo;

implementation

resourcestring
  StrStationary = 'Stationary - Command ID: %d';
  StrMoving = 'Moving';
  StrMovingSComman = 'Moving - %s - Command ID: %d';

{$R *.fmx}

{ TForm4 }

procedure TfrmDobotDemo.actConnectExecute(Sender: TObject);
begin
  FDobotController.Connect(edtSerialPort.Text);

  tmrDobot.Enabled := True;

  actConnect.Enabled := False;
  actDisconnect.Enabled := True;
  layMain.Enabled := True;
end;

procedure TfrmDobotDemo.OnLog(Sender: TObject; const LogText: String; const LogLevel: TDobotLogLevel);
begin
  if LogLevel <= TDobotLogLevel.Debug1 then
  begin
    memLog.BeginUpdate;
    try
      memLog.Lines.Add(LogText);
      memLog.GoToTextEnd;
    finally
      memLog.EndUpdate;
    end;
  end;
end;

procedure TfrmDobotDemo.actDisconnectExecute(Sender: TObject);
begin
  FDobotController.Disconnect;

  tmrDobot.Enabled := False;

  actConnect.Enabled := True;
  actDisconnect.Enabled := False;
  layMain.Enabled := False;
end;

procedure TfrmDobotDemo.actGripperCloseExecute(Sender: TObject);
begin
  FDobotController.SetGripperPosition(TGripperPosition.Close);
end;

procedure TfrmDobotDemo.actGripperOffExecute(Sender: TObject);
begin
  FDobotController.SetGripperPosition(TGripperPosition.Off);
end;

procedure TfrmDobotDemo.actGripperOpenExecute(Sender: TObject);
begin
  FDobotController.SetGripperPosition(TGripperPosition.Open);
end;

procedure TfrmDobotDemo.actHomeExecute(Sender: TObject);
begin
  FDobotController.Home;
end;

procedure TfrmDobotDemo.ActionList1Update(Action: TBasicAction; var Handled: Boolean);
begin
  actScriptAddMove.Enabled :=
    (edtScriptX.Text <> '') and
    (edtScriptY.Text <> '') and
    (edtScriptZ.Text <> '') and
    (edtScriptR.Text <> '');

  actScriptStart.Enabled := not FDobotController.ScriptActive;
  actScriptStop.Enabled := FDobotController.ScriptActive;

  Handled := True;
end;

procedure TfrmDobotDemo.actMoveToPointExecute(Sender: TObject);
begin
  FDobotController.SetPTP(
    TDobotPTPMode(cbPTPMode.ItemIndex),
    TDobotUtils.StrToFloatDefUseDot(edtPTPX.Text, 0),
    TDobotUtils.StrToFloatDefUseDot(edtPTPY.Text, 0),
    TDobotUtils.StrToFloatDefUseDot(edtPTPZ.Text, 0),
    TDobotUtils.StrToFloatDefUseDot(edtPTPR.Text, 0));
end;

procedure TfrmDobotDemo.actScriptAddMoveExecute(Sender: TObject);
begin
  AddScriptLine(format('%s \X %s \Y %s \Z %s \R %s', [
    DobotScriptPTPMode[TDobotPTPMode(cbScriptPTPMode.ItemIndex)],
    edtScriptX.Text,
    edtScriptY.Text,
    edtScriptZ.Text,
    edtScriptR.Text]));
end;

procedure TfrmDobotDemo.actScriptExecQueueExecute(Sender: TObject);
begin
  AddScriptLine('ExecQueue');
end;

procedure TfrmDobotDemo.actScriptHomeExecute(Sender: TObject);
begin
  AddScriptLine('Home');
end;

procedure TfrmDobotDemo.actScriptSessionHomeExecute(Sender: TObject);
begin
  AddScriptLine('Home \Session');
end;

procedure TfrmDobotDemo.actScriptStartExecute(Sender: TObject);
begin
  FDobotController.Text := memScript.Lines;
  FDobotController.ScriptStart;
end;

procedure TfrmDobotDemo.actScriptStopExecute(Sender: TObject);
begin
  FDobotController.ScriptStop;
end;

procedure TfrmDobotDemo.actScriptWaitUntilIdleExecute(Sender: TObject);
begin
  AddScriptLine('WaitIdle');
end;

procedure TfrmDobotDemo.AddScriptLine(const Text: String);
var
  Line: String;
begin
  Line := Text;
  memScript.Lines.Add(Line);
end;

procedure TfrmDobotDemo.actSetPointExecute(Sender: TObject);
begin
  case TDobotPTPMode(cbPTPMode.ItemIndex) of
    MoveJointXYZ,
    MoveJointAngle,
    MoveJointAngleIncrement,
    MoveJointXYZIncrement:
      begin
        edtPTPX.Text := TDobotUtils.FloatToStrUseDot(FPose.jointAngle[0]);
        edtPTPY.Text := TDobotUtils.FloatToStrUseDot(FPose.jointAngle[1]);
        edtPTPZ.Text := TDobotUtils.FloatToStrUseDot(FPose.jointAngle[2]);
        edtPTPR.Text := TDobotUtils.FloatToStrUseDot(FPose.jointAngle[3]);
      end;
    else
      begin
        edtPTPX.Text := TDobotUtils.FloatToStrUseDot(FPose.x);
        edtPTPY.Text := TDobotUtils.FloatToStrUseDot(FPose.y);
        edtPTPZ.Text := TDobotUtils.FloatToStrUseDot(FPose.z);
        edtPTPR.Text := TDobotUtils.FloatToStrUseDot(FPose.r);
      end;
  end;
end;

procedure TfrmDobotDemo.actStopExecute(Sender: TObject);
begin
  FDobotController.Stop(True);
end;

procedure TfrmDobotDemo.OnMoveJointClick(Sender: TObject);
begin
  FDobotController.Move(TDobotJogCommand(TButton(Sender).Tag), TDobotMoveType.Joint);
end;

procedure TfrmDobotDemo.OnMoveLinearClick(Sender: TObject);
begin
  FDobotController.Move(TDobotJogCommand(TButton(Sender).Tag), TDobotMoveType.Linear, 100, True);
end;

constructor TfrmDobotDemo.Create(AOwner: TComponent);
begin
  inherited;

  FDobotAlarms := TStringList.Create;
  FDobotController := TDobotScript.Create(Self);
  FDobotController.OnLog := OnLog;

  layMain.Enabled := False;
end;

destructor TfrmDobotDemo.Destroy;
begin
  FreeAndNil(FDobotAlarms);

  inherited;
end;

procedure TfrmDobotDemo.tmrDobotTimer(Sender: TObject);
begin
  FDobotController.Loop;

  if not FDobotController.ScriptActive then
  begin
    UpdateDobotPose;
    UpdateDobotAlarms;
  end;
end;

procedure TfrmDobotDemo.UpdateDobotAlarms;
begin
  memAlarms.BeginUpdate;
  try
    DobotAlarmsToStrings(FDobotController.GetAlarms, memAlarms.Lines);
  finally
    memAlarms.EndUpdate;
  end;
end;

procedure TfrmDobotDemo.UpdateDobotPose;
var
  Status: String;
begin
  FDobotController.GetPose(FPose);

  lblPosX.Text := 'X = ' + formatfloat('0.000000', FPose.x);
  lblPosY.Text := 'Y = ' + formatfloat('0.000000', FPose.y);
  lblPosZ.Text := 'Z = ' + formatfloat('0.000000', FPose.z);
  lblPosServo.Text := 'Servo = ' + formatfloat('0.000000', FPose.r);
  lblJoint1.Text := 'J1 = ' + formatfloat('0.000000', FPose.jointAngle[0]);
  lblJoint2.Text := 'J2 = ' + formatfloat('0.000000', FPose.jointAngle[1]);
  lblJoint3.Text := 'J3 = ' + formatfloat('0.000000', FPose.jointAngle[2]);
  lblJoint4.Text := 'J4 = ' + formatfloat('0.000000', FPose.jointAngle[3]);

  edtScriptX.Text := TDobotUtils.FloatToStrUseDot(FPose.x);
  edtScriptY.Text := TDobotUtils.FloatToStrUseDot(FPose.y);
  edtScriptZ.Text := TDobotUtils.FloatToStrUseDot(FPose.z);
  edtScriptR.Text := TDobotUtils.FloatToStrUseDot(FPose.r);

  if not FDobotController.Moving then
  begin
    Status := format(StrStationary, [FDobotController.GetCurrentCommandIndex]);
  end
  else
  begin
    Status := format(StrMovingSComman, [DobotAxisMovementsToString(FDobotController.GetAxisMovements), FDobotController.GetCurrentCommandIndex]);
  end;

  lblMoving.Text := Status;
end;

end.
