unit uWorld;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, System.Generics.Collections,
  Vcl.StdCtrls;

type
  TEnumDirection = (dUp, dDown, dLeft, dRight);

  TBodyIndex = class(TObject)
  protected
    FX, FY: Integer;
  public
    constructor Create(X, Y: Integer);
  end;

  TPosition = class(TObject)
  protected
    X1, X2, Y1, Y2: Integer;
    FWolrdIndex: TBodyIndex;
  public
    constructor Create(pWorldIndexX, pWorldIndexY, pX1, pY1, pX2, pY2: Integer);
  end;

  TWormHeadIndex = class
    X: Integer;
    Y: Integer;
    XEnd: Integer;
    YEnd: Integer;
  end;

  TForm1 = class(TForm)
    Timer1: TTimer;
    backgroud: TImage;
    Timer2: TTimer;
    Label1: TLabel;
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
    FWorldMatrix: array[0..29] of array[0..29] of TPosition;
    FWormPart: TPosition;
    FWormHeadIndex: TWormHeadIndex;

    procedure CreateWorld;
    procedure RemoveLast;
    procedure AddNext(const pWormPart: TPosition);
    procedure MoveWorm;
    procedure PaintPosition;
    procedure RandomPart;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  C_IntervalDown = 50;

implementation

{$R *.dfm}

procedure TForm1.AddNext(const pWormPart: TPosition);
begin
  backgroud.Canvas.Rectangle(pWormPart.X1, pWormPart.Y1, pWormPart.X2, pWormPart.Y2);
  FWormBody.Add(pWormPart);
end;

{procedure TForm1.AddPart;
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
end;}

procedure TForm1.MoveWorm;
begin
  case FDirection of
    dRight:
    begin
      Inc(FWormHeadIndex.X);
    end;
    dLeft:
    begin
      Dec(FWormHeadIndex.X);
    end;
    dDown:
    begin
      Inc(FWormHeadIndex.Y);
    end;
    dUp:
    begin
      Dec(FWormHeadIndex.Y);
    end;
  end;

  if FWormHeadIndex.X > Length(FWorldMatrix)-1 then
    FWormHeadIndex.X := 0;

  if FWormHeadIndex.X = -1 then
    FWormHeadIndex.X := Length(FWorldMatrix)-1;

  if FWormHeadIndex.Y > Length(FWorldMatrix)-1 then
    FWormHeadIndex.Y := 0;

  if FWormHeadIndex.Y = -1 then
    FWormHeadIndex.Y := Length(FWorldMatrix)-1;

  if FDirectionChange then
    FDirectionChange := False;

  if (FWormPart.FWolrdIndex.FX = FWormHeadIndex.X) and
     (FWormPart.FWolrdIndex.FY = FWormHeadIndex.Y) then
  begin
    Timer1.Enabled := False;
    FWormBody.Add(FWormPart);
    if (FWormBody.Count mod 3) = 0 then
      Timer1.Interval := Timer1.Interval - C_IntervalDown;
    MoveWorm;
    RandomPart;
    Timer1.Enabled := True;
  end else
  begin
    PaintPosition();
    RemoveLast();
  end;
end;

procedure TForm1.PaintPosition;
var
  oWormPos: TPosition;
  oNewBodyPart: TPosition;
begin
  oWormPos := TPosition(FWorldMatrix[FWormHeadIndex.X][FWormHeadIndex.Y]);
  backgroud.Canvas.Rectangle(oWormPos.X1, oWormPos.Y1, oWormPos.X2, oWormPos.Y2);
  oNewBodyPart := TPosition.Create(FWormHeadIndex.X, FWormHeadIndex.Y, oWormPos.X1, oWormPos.Y1, oWormPos.X2, oWormPos.Y2);
  FWormBody.Add(oNewBodyPart);
end;

procedure TForm1.CreateWorld;
var
  i, y: Integer;
  PosX, PosY: Integer;
  oPos: TPosition;
begin
  PosX := 1;
  PosY := 1;
  for i := 0 to Length(FWorldMatrix) do
  begin
    PosY := 0;
    for y := 0 to Length(FWorldMatrix[i]) do
    begin
      FWorldMatrix[i][y] := TPosition.Create(i, y, PosX, PosY, PosX + 8, PosY + 8);
      PosY := PosY + 9;
    end;
    PosX := PosX + 9;
  end;

  PosX := 0;
  PosY := 0;
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

  backgroud.Canvas.Brush.Color := clBlack;
  backgroud.Canvas.Brush.Style := bsSolid;
  backgroud.Canvas.Pen.Mode := pmCopy;
  backgroud.Canvas.Pen.Color := clBlack;

  CreateWorld();
  FWormHeadIndex := TWormHeadIndex.Create;
  FWormHeadIndex.X := 1;
  FWormHeadIndex.Y := 1;

  PaintPosition;
  Inc(FWormHeadIndex.Y);
  PaintPosition;
  Inc(FWormHeadIndex.Y);
  PaintPosition;

  RandomPart;
end;

procedure TForm1.RandomPart;
var
  randomX, randomY: Integer;
  oPosition: TPosition;
  nextRandom: Boolean;
begin
  nextRandom := True;
  while nextRandom do
  begin
    randomX := Random(29);
    randomY := Random(29);
    if randomX = 0 then randomX := 1;
    if randomY = 0 then randomY := 1;
    oPosition := FWorldMatrix[randomX][randomY];
    nextRandom := FWormBody.Contains(oPosition);
  end;

  backgroud.Canvas.Rectangle(oPosition.X1, oPosition.Y1, oPosition.X2, oPosition.Y2);
  FWormPart := oPosition;
end;

procedure TForm1.RemoveLast;
var
  oRemovePart: TPosition;
  oPenConf: TPenMode;
begin
  oPenConf := backgroud.Canvas.Pen.Mode;
  backgroud.Canvas.Pen.Mode := pmNotXor;
  oRemovePart := FWormBody.First;
  if Assigned(oRemovePart) then
  begin
    backgroud.Canvas.Rectangle(oRemovePart.X1, oRemovePart.Y1, oRemovePart.X2, oRemovePart.Y2);
    backgroud.Canvas.Pen.Mode := oPenConf;
    FWormBody.Remove(oRemovePart);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  MoveWorm;
  //RemoveLast;
  //RandomPart;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  Timer1.Enabled := False;
  if Timer1.Interval > 100 then
    Timer1.Interval := Timer1.Interval - C_IntervalDown;
  Timer1.Enabled := True;
end;

{ TPosition }

constructor TPosition.Create(pWorldIndexX, pWorldIndexY, pX1, pY1, pX2, pY2: Integer);
begin
  inherited Create;

  X1 := pX1;
  X2 := pX2;
  Y1 := pY1;
  Y2 := pY2;

  FWolrdIndex := TBodyIndex.Create(pWorldIndexX, pWorldIndexY);
end;

{ TBodyIndex }

constructor TBodyIndex.Create(X, Y: Integer);
begin
  FX := X;
  FY := Y;
end;

end.
