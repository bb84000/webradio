unit equalizer1;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  ComCtrls, StdCtrls, BGRAFlashProgressBar;

type

  { TFEqualizer }

  TFEqualizer = class(TForm)
    BtnOK: TBitBtn;
    BtnCancel: TBitBtn;
    CBEqualize: TCheckBox;
    LReset: TLabel;
    LdB: TLabel;
    LdB1: TLabel;
    LdB2: TLabel;
    LdB3: TLabel;
    LdB4: TLabel;
    LdB5: TLabel;
    LdB6: TLabel;
    LdB7: TLabel;
    LdB8: TLabel;
    LdB9: TLabel;
    LHz2: TLabel;
    LHz3: TLabel;
    LHz4: TLabel;
    LHz5: TLabel;
    LHz6: TLabel;
    LHz7: TLabel;
    LHz8: TLabel;
    LHz9: TLabel;
    LHzCaption: TLabel;
    LHz1: TLabel;
    LZero: TLabel;
    Panel1: TPanel;
    SBReset: TSpeedButton;
    TBEq1: TTrackBar;
    TBEq2: TTrackBar;
    TBEq3: TTrackBar;
    TBEq5: TTrackBar;
    TBEq7: TTrackBar;
    TBEq8: TTrackBar;
    TBEq9: TTrackBar;
    TBEq4: TTrackBar;
    TBEq6: TTrackBar;
    procedure SBResetClick(Sender: TObject);
    procedure TBEqChange(Sender: TObject);
  private

  public

  end;

var
  FEqualizer: TFEqualizer;

implementation

{$R *.lfm}

{ TFEqualizer }

procedure TFEqualizer.TBEqChange(Sender: TObject);
  var
  NomEq: string;
  NumEq: Integer;
  TB: TTrackBar;
  TLAB: TLabel;
begin
  TB:= TTrackBar(Sender);
  NomEq:= Copy(TB.name, 5, 1);    //Nom du bouton
  NumEq:= StrToInt(NomEq);
  TLAB:= FindComponent('LdB'+NomEq) as TLabel;
  TLAB.Caption:= INttoStr(TB.Position-15);
end;



procedure TFEqualizer.SBResetClick(Sender: TObject);
var
  i: integer;
  TTB: TTrackbar;
begin
  for i:= 1 to 9 do
  begin
    TTB:= FindComponent('TBEq'+InttoStr(i)) as TTrackBar;
    TTB.Position:= 15;
  end;
end;

end.

