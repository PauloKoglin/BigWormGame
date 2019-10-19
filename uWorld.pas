unit uWorld;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, System.Generics.Collections;

type
  TEnumDirection = (dUp, dDown, dLeft, dRight);

  TPosition = class(TObject)
  protected
    X1, X2, Y1, Y2: Integer;
  public
    constructor Create(pX1, pY1, pX2, pY2: Integer);
  end;

  TForm1 = class(TForm)
    Timer1: TTimer;
    backgroud: TImage;
    Timer2: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
    FDirection: TEnumDirection;
    FPrevDirection: TEnumDirection;
    FDirectionChange: Boolean;
    FWormHeadLeft, FWormHeadTop: Integer;
    FWormBody: TObjectList<TPosition>;

    procedure RemoveLast;
    procedure AddNext(const pWormPart: TPosition);
    procedure AddPart;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  C_IntervalDown = 100;

implementation

{$R *.dfm}

procedure TForm1.AddNext(const pWormPart: TPosition);
begin
  backgroud.Canvas.Rectangle(pWormPart.X1, pWormPart.Y1, pWormPart.X2, pWormPart.Y2);
  FWormBody.Add(pWormPart);
end;

procedure TForm1.AddPart;
var
  nLeft, nTop: Integer;
  oWormPart: TPosition;
begin
  case FDirection of
    dRight:
    begin
      if (FPrevDirection in [dUp, dDown]) and (FDirectionChange) then
      begin
        if FPrevDirection = dUp then
          FWormHeadTop := FWormHeadTop + 8;

        if FPrevDirection = dDown then
          FWormHeadTop := FWormHeadTop - 8;

        FWormHeadLeft := FWormHeadLeft + 9;
      end
      else
        FWormHeadLeft := FWormHeadLeft + 1;
    end;

    dLeft:
    begin
      if (FPrevDirection in [dUp, dDown]) and (FDirectionChange) then
      begin
        if FPrevDirection = dUp then
          FWormHeadTop := FWormHeadTop + 8;

        if FPrevDirection = dDown then
          FWormHeadTop := FWormHeadTop - 8;

        FWormHeadLeft := FWormHeadLeft - 1;
      end
      else
        FWormHeadLeft := FWormHeadLeft - 1;
    end;
    dDown:
    begin
      if (FPrevDirection in [dRight, dLeft]) and (FDirectionChange) then
      begin
        if FPrevDirection = dRight then
          FWormHeadLeft := FWormHeadLeft - 8;

        FWormHeadTop := FWormHeadTop + 9;
      end
      else
        FWormHeadTop := FWormHeadTop + 1;
    end;
    dUp:
    begin
      if (FPrevDirection in [dRight, dLeft]) and (FDirectionChange) then
      begin
        if FPrevDirection = dRight then
          FWormHeadLeft := FWormHeadLeft - 8;

        FWormHeadTop := FWormHeadTop - 9;
      end
      else
        FWormHeadTop := FWormHeadTop - 1;
    end;
  end;

  if FDirectionChange then
    FDirectionChange := False;

  nLeft := FWormHeadLeft + 8;
  nTop := FWormHeadTop + 8;

  if FDirection = dLeft then
    nLeft := FWormHeadLeft - 8;

  //oWormPart := TPosition.Create(FWormHeadLeft, FWormHeadTop, nLeft, nTop);
  AddNext(TPosition.Create(FWormHeadLeft, FWormHeadTop, nLeft, nTop));

  case FDirection of
    dRight: FWormHeadLeft := FWormHeadLeft + 8;
    dLeft: FWormHeadLeft := FWormHeadLeft - 8;
    dDown: FWormHeadTop := FWormHeadTop + 8;
    dUp: FWormHeadTop := FWormHeadTop - 8;
  end;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FDirectionChange := True;
  FPrevDirection := FDirection;
  case Key of
    VK_LEFT:
    begin
      if FDirection <> dRight then
        FDirection := dLeft;
    end;
    VK_RIGHT:
    begin
      if FDirection <> dLeft then
        FDirection := dRight;
    end;
    VK_UP:
    begin
      if FDirection <> dDown then
        FDirection := dUp;
    end;
    VK_DOWN:
    begin
      if FDirection <> dUp then
        FDirection := dDown;
    end;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  FWormBody := TObjectList<TPosition>.Create;
  FDirection := dDown;
  FPrevDirection := dDown;
  FDirectionChange := False;
  FWormHeadLeft := 0;
  FWormHeadTop := 0;
  backgroud.Canvas.Brush.Color := clBlack;
  backgroud.Canvas.Brush.Style := bsSolid;
  backgroud.Canvas.Pen.Mode := pmCopy;
  backgroud.Canvas.Pen.Color := clBlack;
  AddPart;
  AddPart;
  AddPart;
end;

procedure TForm1.RemoveLast;
var
  oRemovePart: TPosition;
  oPenConf: TPenMode;
begin
  oPenConf := backgroud.Canvas.Pen.Mode;
  backgroud.Canvas.Pen.Mode := pmNotXor;
  oRemovePart := FWormBody.First;
  backgroud.Canvas.Rectangle(oRemovePart.X1, oRemovePart.Y1, oRemovePart.X2, oRemovePart.Y2);
  backgroud.Canvas.Pen.Mode := oPenConf;
  FWormBody.Remove(oRemovePart);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  AddPart;
  RemoveLast;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if Timer1.Interval > 100 then
    Timer1.Interval := Timer1.Interval - C_IntervalDown;
  Timer1.Enabled := True;
end;

{ TPosition }

constructor TPosition.Create(pX1, pY1, pX2, pY2: Integer);
begin
  inherited Create;

  X1 := pX1;
  X2 := pX2;
  Y1 := pY1;
  Y2 := pY2;
end;

end.
