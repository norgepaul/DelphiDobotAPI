unit Dobot.Dll.Types;

interface

type
  TAlarmArray = array [ 0..31 ] of Byte;
  PAlarmArray = ^TAlarmArray;

  TPose = packed record
    x, y, z, r: single;
    jointAngle: array [ 0..3 ] of single;
  end;
  PPose = ^TPose;

  TKinematics = packed record
    velocity,
    acceleration: single;
  end;
  PKinematics = ^TKinematics;

  THomeParams = packed record
    x, y, z, r: single;
  end;
  PHomeParams = ^THomeParams;

  THomeCmd = packed record
    reserved: LongWord;
  end;
  PHomeCmd = ^THomeCmd;

  TJogJointParams = packed record
    velocity: array [ 0..3 ] of single;
    acceleration: array [ 0..3 ] of single;
  end;
  PJogJointParams = ^TJogJointParams;

  TJOGCoordinateParams = packed record
    velocity: array [ 0..3 ] of single;
    acceleration: array [ 0..3 ] of single;
  end;
  PJOGCoordinateParams = ^TJOGCoordinateParams;

  TJOGCommonParams = packed record
    velocityRatio: single;
    accelerationRatio: single;
  end;
  PJOGCommonParams = ^TJOGCommonParams;

  TJogCmd = packed record
    isJoint: Byte;
    cmd: Byte;
  end;
  PJogCmd = ^TJogCmd;

  TPTPJointParams = packed record
    velocity: array [ 0..3 ] of single;
    acceleration: array [ 0..3 ] of single;
  end;
  PPTPJointParams = ^TPTPJointParams;

  TPTPCoordinateParams = packed record
    xyzVelocity,
    rVelocity,
    xyzAcceleration,
    rAcceleration: single;
  end;
  PPTPCoordinateParams = ^TPTPCoordinateParams;

  TEndEffectorParams = packed record
    xBias: single;
    yBias: single;
    zBias: single;
  end;
  PEndEffectorParams = ^TEndEffectorParams;

  TPTPJumpParams = packed record
    jumpHeight: single;
    zLimit: single;
  end;
  PPTPJumpParams = ^TPTPJumpParams;

  TPTPCommonParams = packed record
    velocityRatio: single;
    accelerationRatio: single;
  end;
  PPTPCommonParams = ^TPTPCommonParams;

{  PTPMode = (
    PTPJUMPXYZMode,
    PTPMOVJXYZMode,
    PTPMOVLXYZMode,

    PTPJUMPANGLEMode,
    PTPMOVJANGLEMode,
    PTPMOVLANGLEMode,

    PTPMOVJANGLEINCMode,
    PTPMOVLXYZINCMode,
    PTPMOVJXYZINCMode,

    PTPJUMPMOVLXYZMode );
 }
  TPTPCmd = packed record
    ptpMode: Byte;
    x: single;
    y: single;
    z: single;
    r: single;
  end;
  PPTPCmd = ^TPTPCmd;

  TCPParams = packed record
    planAcc: single;
    juncitionVel: single;

    temp: single;
{    case Integer of
    0: ( acc: single );
    1: ( period: single );
 }
    realTimeTrack: Byte;
  end;
  PCPParams = ^TCPParams;

  TCPCmd = packed record
    cpMode: Byte;
    x, y, z: single;
    temp: single;

  end;
  PCPCmd = ^TCPCmd;

  TARCParams = packed record
    xyzVelocity,
    rVelocity,
    xyzAcceleration,
    rAcceleration: single;
  end;
  PARCParams = ^TARCParams;

  TwaitCmd = packed record
    timeout: LongWord;
  end;
  PwaitCmd = ^TwaitCmd;

  TARCCmd = packed record
    cirPoint: array [ 0..3 ] of single;
    toPoint: array [ 0..3 ] of single;
  end;
  PARCCmd = ^TARCCmd;

implementation

end.
