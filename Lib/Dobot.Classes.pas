unit Dobot.Classes;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,

  Dobot.Utils,
  Dobot.Interfaces;

type
  EDobotError = class(Exception);
  EDobotConnectError = class(EDobotError);
  EDobotCommandError = class(EDobotError);
  EDobotBusyError = class(EDobotError);
  EDobotMissingParamError = class(EDobotError);
  EDobotScriptError = class(EDobotError);

  TDobotCommandParser = class(TInterfacedObject, IDobotCommandParser)
  strict private
    FCommandText: String;
    FCommand: String;
    FParams: TList<TPair<String, String>>;
    FDelimiters: TArray<Char>;
  private
    procedure ParseCommand;
    function StripDelimiterFromIdentifier(var Value: String): Boolean;
  public
    constructor Create(const CommandText: String); overload;
    constructor Create(const CommandText: String; const Delimiters: TArray<Char>); overload;
    destructor Destroy; override;

    function Command: String;
    function CommandIs(const Value: String): Boolean;
    function GetParam(const Name: String; out Value: String): Boolean; overload;
    function GetParam(const Name: String): String; overload;
    function ParamExists(const Name: String): Boolean;
  end;

implementation

resourcestring
  StrParmmeterSNotF = 'Parmmeter "%s" not found';

{ TDobotCommandParser }

function TDobotCommandParser.Command: String;
begin
  Result := FCommand;
end;

function TDobotCommandParser.CommandIs(const Value: String): Boolean;
begin
  Result := AnsiSameText(Value, FCommand);
end;

constructor TDobotCommandParser.Create(const CommandText: String; const Delimiters: TArray<Char>);
begin
  FDelimiters := Delimiters;

  FParams := TList<TPair<String, String>>.Create;
  FCommandText := CommandText;

  ParseCommand;
end;

constructor TDobotCommandParser.Create(const CommandText: String);
begin
  Create(CommandText, ['\']);
end;

destructor TDobotCommandParser.Destroy;
begin
  FreeAndNil(FParams);

  inherited;
end;

function TDobotCommandParser.GetParam(const Name: String): String;
begin
  if not GetParam(Name, Result) then
  begin
    raise EDobotMissingParamError.CreateFmt(StrParmmeterSNotF, [Name]);
  end;
end;

function TDobotCommandParser.GetParam(const Name: String; out Value: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  for i := 0 to pred(FParams.Count) do
  begin
    if FParams[i].Key = AnsiUpperCase(Name) then
    begin
      Value := FParams[i].Value;

      Exit(True)
    end;
  end;
end;

function TDobotCommandParser.ParamExists(const Name: String): Boolean;
var
  Dummy: String;
begin
  Result := GetParam(Name, Dummy);
end;

function TDobotCommandParser.StripDelimiterFromIdentifier(var Value: String): Boolean;
var
  i: Integer;
begin
  Result := False;

  if Length(Value) > 0 then
  begin
    for i := Low(FDelimiters) to High(FDelimiters) do
    begin
      if Value[low(Value)] = FDelimiters[i] then
      begin
        Result := True;

        Delete(Value, low(Value), 1);

        Break;
      end;
    end;
  end;
end;

procedure TDobotCommandParser.ParseCommand;
var
  Text, Value: String;
  Pair: TPair<String, String>;
begin
  Text := FCommandText;

  FCommand := Trim(TDobotUtils.NextBlock(Text, ' '));

  while Text <> '' do
  begin
    Value := Trim(TDobotUtils.NextBlock(Text));

    if StripDelimiterFromIdentifier(Value) then
    begin
      Pair.Key := AnsiUpperCase(Value);
      Pair.Value := '';

      FParams.Add(Pair);
    end
    else
    begin
      if (FParams.Count >= 0) and
         (FParams[FParams.Count - 1].Value = '') then
      begin
        Pair.Value := Value;

        FParams[FParams.Count - 1] := Pair;
      end;
    end;
  end;
end;

end.
