unit Dobot.Script;

interface

uses
  System.SysUtils, System.Classes, System.RTTI,

  Dobot.Types,
  Dobot.Controller;

type
  TDobotScriptOnError = procedure(Sender: TObject; const e: Exception) of object;
  TDobotScriptOnExecuteLine = procedure(Sender: TObject; const LineNumber: Integer; var LineText: String) of object;

  TDobotScript = class(TBaseDobotController)
  strict private
    FOnStartScript: TNotifyEvent;
    FOnFinishScript: TNotifyEvent;
    FOnExecuteLine: TDobotScriptOnExecuteLine;
    FOnError: TDobotScriptOnError;

    FActive: Boolean;
    FText: TStrings;

    procedure SetText(const Value: TStrings);
  protected
    procedure DoLoop; override;

    procedure DoExecuteLine(const Line: String); virtual;
    procedure DoExecuteScript; virtual;
    procedure DoOnStartScript; virtual;
    procedure DoOnFinishScript; virtual;
    procedure DoDobotScriptOnError(const e: Exception); virtual;
    procedure DoDobotScriptOnExecuteLine(const LineNumber: Integer; var LineText: String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property OnStartScript: TNotifyEvent read FOnStartScript write FOnStartScript;
    property OnFinishScript: TNotifyEvent read FOnFinishScript write FOnFinishScript;
    property OnExecuteLine: TDobotScriptOnExecuteLine read FOnExecuteLine write FOnExecuteLine;
    property OnError: TDobotScriptOnError read FOnError write FOnError;

    property Text: TStrings read FText write SetText;
  end;

implementation

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

procedure TDobotScript.DoExecuteLine(const Line: String);
begin

end;

procedure TDobotScript.DoExecuteScript;
begin

end;

procedure TDobotScript.DoLoop;
begin
  inherited;


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

procedure TDobotScript.SetText(const Value: TStrings);
begin
  FText := Value;
end;

end.
