unit Dobot.Controller;

interface

uses
  System.SysUtils, System.Classes,

  Dobot.Classes,
  Dobot.DLL.Types,
  Dobot.DLL,
  Dobot.Types;

type
  TOnLog = procedure(Sender: TObject; const LogText: String; const LogLevel: TDobotLogLevel) of object;

  TDobotController = class(TComponent)
  private
    FOnLog: TOnLog;
    FNextTimerTicks: Cardinal;
    FConnected: Boolean;

    procedure CheckTimer;
  protected
    FCmdIndex: UInt64;

    procedure NextCommandIndex;
    procedure CheckResult(const Value: Integer);

    procedure DoLog(const LogText: String; const LogLevel: TDobotLogLevel = TDobotLogLevel.Info); virtual;
    procedure DoOnTimer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Loop; // Call every 100ms

    procedure Connect(const SerialPort: String = ''; const BaudRate: Integer = 115200; const Timeout: Integer = 3000);
    procedure Disconnect;
    function Connected: Boolean;

    procedure SetCoordinateJOGParameters(const Velocity, Acceleration: Single);
    procedure SetJointJOGParameters(const Velocity, Acceleration: Single);
    procedure SetPTPJointParameters(const Velocity, Acceleration: Single);
    procedure SetJogCommonParameters(const VelocityRatio, AccelerationRatio: Single);
    procedure SetPTPCommonParameters(const VelocityRatio, AccelerationRatio: Single);
    procedure SetPTPJumpParameters(const jumpHeight, zLimit: Single);
    procedure SetPTPCoordinateParameters(const xyzVelocity, rVelocity, xyzAcceleration, rAcceleration: Single);
    procedure SetCommandTimeout(const Value: Integer);
    procedure SetQueuedCmdClear;
    procedure SetQueuedCmdStartExec;
    procedure GetDeviceVersion(out Major, Minor, Rev: Byte);
    procedure GetPose(out Pose: TPose);
    procedure GetAlarmState(out AlarmState: TArray<Byte>);
    function GetAlarms: TDobotAlarms;
    function GetCurrentCommandIndex: UInt64;

    procedure Exec;
    procedure Home;
    procedure Stop(const Force: Boolean);

    procedure SetPTP(const Mode: TDobotPTPMode; const X, Y, Z, R: Single);

    procedure SetGripperPosition(const GripperPosition: TGripperPosition);

    procedure Move(const Command: TDobotJogCommand; const MoveType: TDobotMoveType; const MoveForMS: Cardinal = 100);
  published
    property OnLog: TOnLog read FOnLog write FOnLog;
  end;

implementation

resourcestring
  StrErrorCode = 'Error code: ';

procedure TDobotController.CheckResult(const Value: Integer);
begin
  NextCommandIndex;

  if Value <> 0 then
  begin
    DoLog(StrErrorCode + Value.ToString);

    raise EDobotCommandError.Create(StrErrorCode + Value.ToString);
  end;
end;

procedure TDobotController.SetJointJOGParameters(const Velocity, Acceleration: Single);
var
  i: Integer;
  jointJOGParams: TJOGJointParams;
begin
  for i := 0 to 3 do
  begin
    jointJOGParams.velocity[i] := Velocity;
    jointJOGParams.acceleration[i] := Acceleration;
  end;

  CheckResult(Dobot.DLL.SetJOGJointParams(@jointJOGParams, false, @FCmdIndex));

  DoLog('Joint jog parameters set', TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetCoordinateJOGParameters(const Velocity, Acceleration: Single);
var
  i: Integer;
  coordinateJOGParams: TJOGcoordinateParams;
begin
  for i := 0 to 3 do
  begin
    coordinateJOGParams.velocity[i] := Velocity;
    coordinateJOGParams.acceleration[i] := Acceleration;
  end;

  CheckResult(Dobot.DLL.SetJOGCoordinateParams(@coordinateJOGParams, false, @FCmdIndex));

  DoLog('Coordinate jog parameters set', TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetGripperPosition(const GripperPosition: TGripperPosition);
begin
  case GripperPosition of
    gpOff: CheckResult(Dobot.DLL.SetEndEffectorGripper(False, False, False, @FCmdIndex));
    gpOpen: CheckResult(Dobot.DLL.SetEndEffectorGripper(True, False, False, @FCmdIndex));
    gpClosed: CheckResult(Dobot.DLL.SetEndEffectorGripper(True, True, False, @FCmdIndex));
  end;
end;

procedure TDobotController.SetPTPJointParameters(const Velocity, Acceleration: Single);
var
  i: Integer;
  ptpJointParams: TptpJointParams;
begin
  for i := 0 to 3 do
  begin
    ptpJointParams.velocity[i] := Velocity;
    ptpJointParams.acceleration[i] := Acceleration;
  end;

  CheckResult(Dobot.DLL.SetPTPJointParams(@ptpJointParams, false, @FCmdIndex));

  DoLog('PTP joint parameters set', TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetJogCommonParameters(const VelocityRatio, AccelerationRatio: Single);
var
  jogcommonParams: TJOGCommonParams;
begin
  jogcommonParams.velocityRatio := VelocityRatio;
  jogcommonParams.accelerationRatio := AccelerationRatio;

  CheckResult(Dobot.DLL.SetJOGCommonParams(@jogcommonParams, false, @FCmdIndex));

  DoLog('Jog common parameters set', TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetPTP(const Mode: TDobotPTPMode; const X, Y, Z, R: Single);
var
  PTPCmd: TPTPCmd;
begin
  PTPCmd.ptpMode := Byte(Mode);

  PTPCmd.x := X;
  PTPCmd.y := Y;
  PTPCmd.z := Z;
  PTPCmd.r := R;

  CheckResult(Dobot.DLL.SetPTPCmd(@PTPCmd, false, @FCmdIndex));
end;

procedure TDobotController.SetPTPCommonParameters(const VelocityRatio, AccelerationRatio: Single);
var
  ptpCommonParams: TptpCommonParams;
begin
  ptpCommonParams.velocityRatio := VelocityRatio;
  ptpCommonParams.accelerationRatio := AccelerationRatio;

  CheckResult(Dobot.DLL.SetPTPCommonParams(@ptpCommonParams, false, @FCmdIndex));

  DoLog('PTP common parameters set', TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetPTPCoordinateParameters(const xyzVelocity, rVelocity, xyzAcceleration, rAcceleration: Single);
var
  ptpCoordinateParams: TptpCoordinateParams;
begin
  ptpCoordinateParams.xyzVelocity := xyzVelocity;
  ptpCoordinateParams.rVelocity := rVelocity;
  ptpCoordinateParams.xyzAcceleration := xyzAcceleration;
  ptpCoordinateParams.rAcceleration := rAcceleration;

  CheckResult(Dobot.DLL.SetPTPCoordinateParams(@ptpCoordinateParams, false, @FCmdIndex));

  DoLog('PTP coordinate parameters set', TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetPTPJumpParameters(const jumpHeight, zLimit: Single);
var
  ptpJumpParams: TptpJumpParams;
begin
  ptpJumpParams.jumpHeight := jumpHeight;
  ptpJumpParams.zLimit := zLimit;

  CheckResult(Dobot.DLL.SetPTPJumpParams(@ptpJumpParams, false, @FCmdIndex));

  DoLog('PTP jump parameters set', TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetCommandTimeout(const Value: Integer);
begin
  CheckResult(Dobot.DLL.SetCmdTimeout(Value));

  DoLog('Command timeout set', TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetQueuedCmdClear;
begin
  CheckResult(Dobot.DLL.SetQueuedCmdClear);

  DoLog('Command queue cleared', TDobotLogLevel.Debug1);
end;

procedure TDobotController.SetQueuedCmdStartExec;
begin
  CheckResult(Dobot.DLL.SetQueuedCmdStartExec);

  DoLog('Command start exec', TDobotLogLevel.Debug1);
end;

procedure TDobotController.Stop(const Force: Boolean);
begin
  if Force then
  begin
    CheckResult(Dobot.DLL.SetQueuedCmdForceStopExec);
  end
  else
  begin
    CheckResult(Dobot.DLL.SetQueuedCmdStopExec);
  end;
end;

procedure TDobotController.GetDeviceVersion(out Major, Minor, Rev: Byte);
var
  amajorV, aminorV, arev: Byte;
begin
  CheckResult(Dobot.DLL.GetDeviceVersion(@amajorV, @aminorV, @arev));

  Major := amajorv;
  Minor := aminorv;
  Rev := arev;

  DoLog(format('Device Version = %d.%d.%d', [Major, Minor, Rev]));
end;

procedure TDobotController.GetPose(out Pose: TPose);
begin
  CheckResult(Dobot.DLL.getPose(@Pose));

  DoLog('Get Device Pose', TDobotLogLevel.Debug3);
end;

procedure TDobotController.Home;
var
  HomeCmd:THomeCmd;
begin
  CheckResult(Dobot.DLL.SetHOMECmd(@HomeCmd, false, @FCmdIndex));
end;

procedure TDobotController.Loop;
begin
  CheckTimer;
end;

procedure TDobotController.CheckTimer;
begin
  if (FNextTimerTicks <> 0) and
     (TThread.GetTickCount >= FNextTimerTicks) then
  begin
    FNextTimerTicks := 0;

    DoOnTimer;
  end;
end;

procedure TDobotController.Move(const Command: TDobotJogCommand; const MoveType: TDobotMoveType; const MoveForMS: Cardinal);
var
  JOGCmd: TJOGCmd;
begin
  JOGCmd.cmd := Byte(Command);
  JOGCmd.isJoint := Byte(MoveType);

  CheckResult(Dobot.DLL.SetJOGCmd(@JOGCmd, false, @FCmdIndex));

  FNextTimerTicks := 0;

  if MoveForMS <> 0 then
  begin
    FNextTimerTicks := TThread.GetTickCount + MoveForMS;
  end;
end;

function TDobotController.GetAlarms: TDobotAlarms;
var
  Bytes: TArray<Byte>;
begin
  GetAlarmState(Bytes);

  Result := DobotAlarmBytesToDobotAlarms(Bytes);
end;

procedure TDobotController.GetAlarmState(out AlarmState: TArray<Byte>);
var
  Len: Integer;
begin
  Len := 32;

  SetLength(AlarmState, Len);

  CheckResult(Dobot.DLL.getAlarmsState(@AlarmState[0], @len, Len));
  CheckResult(Dobot.DLL.ClearAllAlarmsState);
end;

function TDobotController.GetCurrentCommandIndex: UInt64;
begin
  Dobot.DLL.GetQueuedCmdCurrentIndex(@Result);
end;

procedure TDobotController.Connect(const SerialPort: String; const BaudRate: Integer; const Timeout: Integer);
var
  ConnectResult: Integer;
begin
  ConnectResult := Dobot.DLL.ConnectDobot(
    PChar(SerialPort),
    Baudrate
  );

  SetCommandTimeout(Timeout);

  case ConnectResult of
    1: raise EDobotConnectError.Create('Dobot serial port not found');
    2: raise EDobotConnectError.Create('Error connecting to the Dobot serial port');
  end;

  SetJointJOGParameters(200, 200);
  SetCoordinateJOGParameters(200, 200);
  SetPTPJointParameters(200, 200);
  SetJogCommonParameters(50, 50);
  SetPTPCommonParameters(50, 50);
  SetPTPCoordinateParameters(200, 200, 200, 200);
  SetPTPJumpParameters(10, 20);

  SetQueuedCmdClear;
  SetQueuedCmdStartExec;

  FConnected := True;
end;

function TDobotController.Connected: Boolean;
begin
  Result := FConnected;
end;

constructor TDobotController.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TDobotController.Destroy;
begin
  inherited;
end;

procedure TDobotController.Disconnect;
begin
  Dobot.DLL.DisconnectDobot;

  FConnected := False;

  DoLog('Disconnected', TDobotLogLevel.Debug1);
end;

procedure TDobotController.DoLog(const LogText: String; const LogLevel: TDobotLogLevel);
begin
  if Assigned(FOnLog) then
  begin
    FOnLog(Self, LogText, LogLevel);
  end;
end;

procedure TDobotController.DoOnTimer;
begin
  Move(TDobotJogCommand.Release, TDobotMoveType.mtLinear, 0);
end;

procedure TDobotController.Exec;
begin
  CheckResult(Dobot.DLL.DobotExec);
end;

procedure TDobotController.NextCommandIndex;
begin
  Inc(FCmdIndex);
end;

end.
