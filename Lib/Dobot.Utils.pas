unit Dobot.Utils;

interface

uses
  System.SysUtils;

type
  TDobotUtils = class
  public
    class function FloatToStrUseDot(const Value: Double): String;
    class function StrToFloatDefUseDot(const Value: String; const Default: Double): Double;
  end;

implementation

{ TDobotUtils }

class function TDobotUtils.FloatToStrUseDot(const Value: Double): String;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings.DecimalSeparator := '.';

  Result := FloatToStr(Value, FormatSettings);
end;

class function TDobotUtils.StrToFloatDefUseDot(const Value: String; const Default: Double): Double;
var
  FormatSettings: TFormatSettings;
begin
  FormatSettings.DecimalSeparator := '.';

  Result := StrToFloatDef(Value, Default, FormatSettings);
end;

end.
