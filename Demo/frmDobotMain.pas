unit frmDobotMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls, System.Actions,
  FMX.ActnList, FMX.Controls.Presentation, FMX.Layouts, FMX.Edit,
  FMX.ListBox, FMX.ScrollBox, FMX.Memo,

  Dobot.Controller,
  Dobot.Classes,
  Dobot.Types,
  Dobot.Utils,
  Dobot.DLL.Types, FMX.TabControl;

type
  TfrmDobotDemo = class(TForm)
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Button1: TButton;
    ActionList1: TActionList;
    Button2: TButton;
    GroupBox1: TGroupBox;
    Button3: TButton;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
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
    Layout6: TLayout;
    Layout7: TLayout;
    Button4: TButton;
    cbPTPMode: TComboBox;
    Layout8: TLayout;
    edtPTPX: TEdit;
    edtPTPR: TEdit;
    edtPTPZ: TEdit;
    edtPTPY: TEdit;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Button24: TButton;
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
    Layout9: TLayout;
    Label9: TLabel;
    memAlarms: TMemo;
    Splitter1: TSplitter;
    Layout10: TLayout;
    Label10: TLabel;
    memLog: TMemo;
    tmrDobot: TTimer;
    layMain: TLayout;
    Button25: TButton;
    actStop: TAction;
    TabControl1: TTabControl;
    tabGripper: TTabItem;
    Button5: TButton;
    Button8: TButton;
    Button9: TButton;
    Sucker: TTabItem;
    Laser: TTabItem;
    edtSerialPort: TEdit;
    Button26: TButton;
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
    procedure Button26Click(Sender: TObject);
  private
    FDobotController: TDobotController;
    FPose: TPose;
    FDobotAlarms: TStringList;
    procedure UpdateDobotAlarms;
    procedure UpdateDobotPose;
    procedure OnLog(Sender: TObject; const LogText: String; const LogLevel: TDobotLogLevel);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmDobotDemo: TfrmDobotDemo;

implementation

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
  FDobotController.SetGripperPosition(TGripperPosition.gpClosed);
end;

procedure TfrmDobotDemo.actGripperOffExecute(Sender: TObject);
begin
  FDobotController.SetGripperPosition(TGripperPosition.gpOff);
end;

procedure TfrmDobotDemo.actGripperOpenExecute(Sender: TObject);
begin
  FDobotController.SetGripperPosition(TGripperPosition.gpOpen);
end;

procedure TfrmDobotDemo.actHomeExecute(Sender: TObject);
begin
  FDobotController.Home;
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

procedure TfrmDobotDemo.actSetPointExecute(Sender: TObject);
begin
  case TDobotPTPMode(cbPTPMode.ItemIndex) of
    MoveJointXYZ,
    MoveJointAngle,
    MoveJointIncrement:
      begin
        edtPTPX.Text := TDobotUtils.FloatToStrUseDot(FPose.jointAngle[0]);
        edtPTPY.Text := TDobotUtils.FloatToStrUseDot(FPose.jointAngle[1]);
        edtPTPZ.Text := TDobotUtils.FloatToStrUseDot(FPose.jointAngle[2]);
        edtPTPR.Text := TDobotUtils.FloatToStrUseDot(FPose.jointAngle[3]);
      end;

    JumpXYZ,
    JumpAngle,
    MoveLinearAngle,
    MoveLinearXYZ,
    MoveLinearIncrement:
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

procedure TfrmDobotDemo.Button26Click(Sender: TObject);
begin
  Label10.Text := FDobotController.GetCurrentCommandIndex.ToString;
end;

procedure TfrmDobotDemo.OnMoveJointClick(Sender: TObject);
begin
  FDobotController.Move(TDobotJogCommand(TButton(Sender).Tag), TDobotMoveType.mtJoint);
end;

procedure TfrmDobotDemo.OnMoveLinearClick(Sender: TObject);
begin
  FDobotController.Move(TDobotJogCommand(TButton(Sender).Tag), TDobotMoveType.mtLinear);
end;

constructor TfrmDobotDemo.Create(AOwner: TComponent);
begin
  inherited;

  FDobotAlarms := TStringList.Create;
  FDobotController := TDobotController.Create(Self);
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

  UpdateDobotPose;
  UpdateDobotAlarms;
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
end;

end.
