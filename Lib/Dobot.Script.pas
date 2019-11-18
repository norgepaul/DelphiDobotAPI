unit Dobot.Script;

interface

uses
  System.SysUtils, System.Classes, System.RTTI,

  Dobot.Types,
  Dobot.Interfaces,
  Dobot.Classes,
  Dobot.Utils,
  Dobot.Controller;

type
  TDobotScriptOnError = procedure(Sender: TObject; const e: Exception) of object;
  TDobotScriptOnExecuteLine = procedure(Sender: TObject; const LineNumber: Integer; var LineText: String) of object;

  TDobotScript = class(TDobotController)
  strict private
    FOnStartScript: TNotifyEvent;
    FOnFinishScript: TNotifyEvent;
    FOnExecuteLine: TDobotScriptOnExecuteLine;
    FOnError: TDobotScriptOnError;

    FActive: Boolean;
    FText: TStrings;
    FCurrentLine: Integer;
    FPreviousLine: Integer;
    FState: TDobotScriptState;

    procedure SetText(const Value: TStrings);
  private
    procedure ProcessHome(const Session: Boolean);
    procedure ProcessPTP(const PTPMode: TDobotPTPMode; const X, Y, Z, R: Single);
  protected
    procedure DoLoop; override;
    procedure DoDisconnect; override;

    procedure DoExecuteLine(const Line: String); virtual;
    procedure DoExecuteScript; virtual;
    procedure DoOnStartScript; virtual;
    procedure DoOnFinishScript; virtual;
    procedure DoDobotScriptOnError(const e: Exception); virtual;
    procedure DoDobotScriptOnExecuteLine(const LineNumber: Integer; var LineText: String); virtual;

    procedure NextLine;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ScriptStart;
    procedure ScriptStop;
    function ScriptActive: Boolean;
  published
    property OnStartScript: TNotifyEvent read FOnStartScript write FOnStartScript;
    property OnFinishScript: TNotifyEvent read FOnFinishScript write FOnFinishScript;
    property OnExecuteLine: TDobotScriptOnExecuteLine read FOnExecuteLine write FOnExecuteLine;
    property OnError: TDobotScriptOnError read FOnError write FOnError;

    property Text: TStrings read FText write SetText;
  end;

implementation

resourcestring
  StrSyntaxErrorAtLine = 'Syntax Error at line %d - %s';
  StrDobotAlarmed = 'Dobot alarmed - ';

{ TDobotScript }

constructor TDobotScript.Create(AOwner: TComponent);
begin
  inherited;

  FText := TStringList.Create;
end;

destructor TDobotScript.Destroy;
begin
  FreeAndNil(FText);

  inherited;
end;

procedure TDobotScript.DoDisconnect;
begin
  ScriptStop;

  inherited;
end;

procedure TDobotScript.DoDobotScriptOnError(const e: Exception);
begin
  if Assigned(FOnError) then
  begin
    FOnError(Self, e);
  end;
end;

procedure TDobotScript.DoDobotScriptOnExecuteLine(const LineNumber: Integer; var LineText: String);
begin
  if Assigned(FOnExecuteLine) then
  begin
    FOnExecuteLine(Self, LineNumber, LineText);
  end;
end;

procedure TDobotScript.ProcessHome(const Session: Boolean);
begin
  Home;
end;

procedure TDobotScript.ProcessPTP(const PTPMode: TDobotPTPMode; const X, Y, Z, R: Single);
begin
  SetPTP(
    PTPMode,
    X,
    Y,
    Z,
    R);
end;

procedure TDobotScript.DoExecuteLine(const Line: String);
var
  Parser: IDobotCommandParser;
  PTPMode: TDobotPTPMode;
begin
  Parser := TDobotCommandParser.Create(Line);

  if Line = '' then
  begin
    NextLine;
  end else
  if Parser.CommandIs('Home') then
  begin
    ProcessHome(
      Parser.ParamExists('session'));

    NextLine;
  end else
  if DobotScriptPTPModeTextToPTPMode(Parser.Command, PTPMode) then
  begin
    ProcessPTP(
      PTPMode,
      TDobotUtils.StrToFloatDefUseDot(Parser.GetParam('X'), 0),
      TDobotUtils.StrToFloatDefUseDot(Parser.GetParam('Y'), 0),
      TDobotUtils.StrToFloatDefUseDot(Parser.GetParam('Z'), 0),
      TDobotUtils.StrToFloatDefUseDot(Parser.GetParam('R'), 0));

    NextLine;
  end else
  if Parser.CommandIs('WaitIdle') then
  begin
    FState := dssWaitingStationary;
  end
  else
  begin
    raise EDobotScriptError.CreateFmt(StrSyntaxErrorAtLine, [FCurrentLine, Line]);
  end;
end;

procedure TDobotScript.DoExecuteScript;
var
  Line: String;
  Alarms: TDobotAlarms;
  AlarmStrings: TStringList;
begin
  Alarms := GetAlarms;

  if Alarms <> [] then
  begin
    AlarmStrings := TStringList.Create;
    try
      DobotAlarmsToStrings(Alarms, AlarmStrings);

      raise EDobotScriptError.Create(StrDobotAlarmed + AlarmStrings.Text);
    finally
      FreeAndNil(AlarmStrings);
    end;
  end;

  case FState of
    dssIdle:
      begin
        FState := dssWorking;

        Line := Trim(FText[FCurrentLine]);

        DoDobotScriptOnExecuteLine(FCurrentLine, Line);

        FPreviousLine := FCurrentLine;

        DoExecuteLine(Line);
      end;

    dssWorking:
      begin
      end;

    dssWaitingStationary:
      begin
        if not Moving then
        begin
          NextLine;
        end;
      end;
  end;
end;

procedure TDobotScript.DoLoop;
begin
  if FActive then
  begin
    try
      DoExecuteScript;
    except
      on e: Exception do
      begin
        ScriptStop;

        DoDobotScriptOnError(e);
      end;
    end;

    inherited;
  end;
end;

procedure TDobotScript.DoOnFinishScript;
begin
  if Assigned(FOnFinishScript) then
  begin
    FOnFinishScript(Self);
  end;
end;

procedure TDobotScript.DoOnStartScript;
begin
  if Assigned(FOnStartScript) then
  begin
    FOnStartScript(Self);
  end;
end;

procedure TDobotScript.NextLine;
begin
  if FCurrentLine < pred(FText.Count) then
  begin
    Inc(FCurrentLine);

    FState := dssIdle;
  end
  else
  begin
    ScriptStop;
  end;
end;

procedure TDobotScript.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
end;

function TDobotScript.ScriptActive: Boolean;
begin
  Result := FActive;
end;

procedure TDobotScript.ScriptStart;
begin
  if not FActive then
  begin
    FPreviousLine := -1;
    FCurrentLine := 0;
    FState := dssIdle;

    DoOnStartScript;

    FActive := True;
  end;
end;

procedure TDobotScript.ScriptStop;
begin
  if FActive then
  begin
    DoOnFinishScript;

    FActive := False;
  end;
end;

end.
