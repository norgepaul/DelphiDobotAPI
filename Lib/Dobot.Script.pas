unit Dobot.Script;

interface

uses
  System.SysUtils, System.Classes, System.RTTI,

  Dobot.Types,
  Dobot.Controller;

type
  TDobotScript = class(TComponent)
  private
    FText: TStrings;

    procedure SetText(const Value: TStrings);
  protected
    procedure DoExecuteScript(const Queued: Boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFunctions;
  published
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

procedure TDobotScript.DoExecuteScript(const Queued: Boolean);
begin

end;

procedure TDobotScript.LoadFunctions;
var
  LContext: TRttiContext;
  LType: TRttiType;
begin
  LContext := TRttiContext.Create;

(*  LContext.GetType(Dobot.Dll);
  try
    for LType in LContext.GetTypes do
    begin
      Writeln(LType.Name);
    end;
  finally
    LContext.Free;
  end;*)
end;

procedure TDobotScript.SetText(const Value: TStrings);
begin
  FText := Value;
end;

end.
