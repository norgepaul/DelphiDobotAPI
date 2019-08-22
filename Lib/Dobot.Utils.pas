unit Dobot.Utils;

interface

uses
  System.SysUtils;

type
  TDobotUtils = class
  public
    class function FloatToStrUseDot(const Value: Double): String;
    class function StrToFloatDefUseDot(const Value: String; const Default: Double): Double;
    class function NextBlock(var Value: String; const Delimiter: String = ' '; const QuotedString: Boolean = False): String;
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

class function TDobotUtils.NextBlock(var Value: String; const Delimiter: String; const QuotedString: Boolean): String;
const
  Quote = '''';
var
  p: Integer;
  InQuotes: Boolean;
begin
  p := 1;

  InQuotes := False;

  while (p <= length(Value) - length(Delimiter) + 1) and
        ((copy(Value, p, length(Delimiter)) <> Delimiter) or
        (InQuotes)) do
  begin
    if Value[p] = Quote then
      InQuotes := not InQuotes;

    Inc(p);
  end;

  if p = length(Value) then
    Result := Value
  else
    Result := copy(Value, 1, p - 1);

  Value := Trim(copy(Value, p + length(Delimiter), MaxInt));

  if (QuotedString) and (Result <> '') then
  begin
    if Result[1] = Quote then
      Delete(Result, 1, 1);

    if (Result <> '') and (Result[length(Result)] = Quote) then
      Delete(Result, length(Result), 1);
  end;
end;

end.
