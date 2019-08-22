unit Dobot.Interfaces;

interface

type
  IDobotCommandParser = interface
    ['{442494BE-C2D5-41F1-9500-5F9C7DF81770}']
    function Command: String;
    function CommandIs(const Value: String): Boolean;
    function GetParam(const Name: String; out Value: String): Boolean; overload;
    function GetParam(const Name: String): String; overload;
    function ParamExists(const Name: String): Boolean;
  end;

implementation

end.
