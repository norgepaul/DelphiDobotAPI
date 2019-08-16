unit Dobot.Classes;

interface

uses
  System.SysUtils;

type
  EDobotError = class(Exception);
  EDobotConnectError = class(EDobotError);
  EDobotCommandError = class(EDobotError);
  EDobotBusyError = class(EDobotError);

implementation

end.
