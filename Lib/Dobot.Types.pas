unit Dobot.Types;

interface

uses
  System.Classes;

type
  TDobotJogCommand = (
    Release,
    XAxisPlus,
    XAxisMinus,
    YAxisPlus,
    YAxisMinus,
    ZAxisPlus,
    ZAxisMinus,
    ServoRotateForward,
    ServoRotateBackward,
    SuctionIn,
    SuctionOut,
    GripperRotateForward,
    GripperRotateBackward,
    LaserOn,
    LaserOff
  );

  TDobotMoveType = (
    Linear,
    Joint
  );

  TGripperPosition = (
    Off,
    Open,
    Close
  );

  TDobotPTPMode = (
    JumpXYZ,
    MoveJointXYZ,
    MoveLinearXYZ,
    JumpAngle,
    MoveJointAngle,
    MoveLinearAngle,
    MoveJointAngleIncrement,
    MoveLinearXYZIncrement,
    MoveJointXYZIncrement,
    JumpMoveXYZ
  );

  TDobotAlarm = (
    Reset,
    UndefinedInstruction,
    FileSystemError,
    CommunicationFailureBetweenMCUAndFPGA,
    AngleSensorReadingError,
    InverseResolveAbnormalAlarm,
    PlanningInverseResolveAlarm,
    PlanningInverseResolveLimitationAlarm,
    PlanningDataRepetitionAlarm,
    PlanningJUMPParameterAlarm,
    MotionInverseResolveSingularityAlarm,
    MotionInverseResolveAlarm,
    MotionInverseResolveLimitationAlarm,
    Joint1Overspeed,
    Joint2Overspeed,
    Joint3Overspeed,
    Joint4Overspeed,
    Joint1PositiveLimitAlarm,
    Joint1NegativeLimitAlarm,
    Joint2PositiveLimitAlarm,
    Joint2NegativeLimitAlarm,
    Joint3PositiveLimitAlarm,
    Joint3NegativeLimitAlarm,
    Joint4PositiveLimitAlarm,
    Joint4NegativeLimitAlarm,
    ParallegramPositiveLimitAlarm,
    ParallegramNegativeLimitAlarm,
    Joint1LosingStep,
    Joint2LosingStep,
    Joint3LosingStep,
    Joint4LosingStep
  );
  TDobotAlarms = set of TDobotAlarm;

  TDobotLogLevel = (
    NoLog,
    Info,
    Warning,
    Error,
    Debug1,
    Debug2,
    Debug3
  );

const
  DobotAlarmIndices: Array[TDobotAlarm] of Byte = (
    $00,
    $01,
    $02,
    $03,
    $04,
    $10,
    $11,
    $12,
    $13,
    $15,
    $20,
    $21,
    $22,
    $30,
    $31,
    $32,
    $33,
    $40,
    $41,
    $42,
    $43,
    $44,
    $45,
    $46,
    $47,
    $48,
    $49,
    $50,
    $51,
    $52,
    $53
  );

  DobotAlarmDescriptions: Array[TDobotAlarm] of String = (
    'Reset',
    'Undefined Instruction',
    'File System Error',
    'Communication Failure Between MCU and FPGA',
    'Angle Sensor Reading Error',
    'Inverse Resolve Abnormal Alarm',
    'Planning Inverse Resolve Alarm',
    'Planning Inverse Resolve Limitation Alarm',
    'Planning Data Repetition Alarm',
    'Planning JUMP Parameter Alarm',
    'MotionInverse Resolve Singularity Alarm',
    'MotionInverse Resolve Alarm',
    'MotionInverse Resolve Limitation Alarm',
    'Joint 1 Overspeed',
    'Joint 2 Overspeed',
    'Joint 3 Overspeed',
    'Joint 4 Overspeed',
    'Joint 1 Positive Limit Alarm',
    'Joint 1 Negative Limit Alarm',
    'Joint 2 Positive Limit Alarm',
    'Joint 2 Negative Limit Alarm',
    'Joint 3 Positive Limit Alarm',
    'Joint 3 Negative Limit Alarm',
    'Joint 4 Positive Limit Alarm',
    'Joint 4 Negative Limit Alarm',
    'Parallegram Positive Limit Alarm',
    'Parallegram Negative Limit Alarm',
    'Joint 1 Losing Step',
    'Joint 2 Losing Step',
    'Joint 3 Losing Step',
    'Joint 4 Losing Step'
  );

  DobotJogCommandDescriptions: Array[TDobotJogCommand] of String = (
    'Release',
    'XAxisPlus',
    'XAxisMinus',
    'YAxisPlus',
    'YAxisMinus',
    'ZAxisPlus',
    'ZAxisMinus',
    'ServoRotateForward',
    'ServoRotateBackward',
    'SuctionIn',
    'SuctionOut',
    'GripperRotateForward',
    'GripperRotateBackward',
    'LaserOn',
    'LaserOff'
  );

  DobotMoveTypeDescriptions: Array[TDobotMoveType] of String = (
    'Linear',
    'Joint'
  );


procedure DobotAlarmsToStrings(const Value: TDobotAlarms; const Strings: TStrings);
function DobotAlarmIndexToDobotAlarm(const AlarmIndex: Byte; out DobotAlarm: TDobotAlarm): Boolean;
function DobotAlarmBytesToDobotAlarms(const AlarmBytes: Array of Byte): TDobotAlarms;

implementation

procedure DobotAlarmsToStrings(const Value: TDobotAlarms; const Strings: TStrings);
var
  DobotAlarm: TDobotAlarm;
begin
  Strings.Clear;

  for DobotAlarm := Low(TDobotAlarm) to High(TDobotAlarm) do
  begin
    if DobotAlarm in Value then
    begin
      Strings.Add(DobotAlarmDescriptions[DobotAlarm]);
    end;
  end;
end;

function DobotAlarmIndexToDobotAlarm(const AlarmIndex: Byte; out DobotAlarm: TDobotAlarm): Boolean;
var
  ADobotAlarm: TDobotAlarm;
begin
  Result := False;

  for ADobotAlarm := Low(TDobotAlarm) to High(TDobotAlarm) do
  begin
    if DobotAlarmIndices[ADobotAlarm] = AlarmIndex then
    begin
      DobotAlarm := ADobotAlarm;

      Exit(True);
    end;
  end;
end;

function DobotAlarmBytesToDobotAlarms(const AlarmBytes: Array of Byte): TDobotAlarms;
var
  i: Integer;
  n: Integer;
  DobotAlarm: TDobotAlarm;
begin
  Result := [];

  for i := Low(AlarmBytes) to High(AlarmBytes) do
  begin
    for n := 0 to 7 do
    begin
      if (AlarmBytes[i] shr n) and $01 = 1 then
      begin
        if DobotAlarmIndexToDobotAlarm((i * 8) + n, DobotAlarm) then
        begin
          Result := Result + [DobotAlarm];
        end;
      end;
    end;
  end;
end;

end.
