unit Rozeta.FormGPFontDialog;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  License: public domain.

  2022.06
}

{$mode delphi}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  JPL.Strings, JPL.Conversion, JPL.Colors,
  JPLazSpinEdit, JPP.ComboBox, JPP.CheckBox, JPP.ColorComboBox, JPP.Timer, JPP.Common.Procs, JPP.Labels, JPP.Edit,
  Rozeta.AppStrings,
  GdiPlus, GdiPlusHelpers;


const
  DEFAULT_EXAMPLE_TEXT = 'ABCD abcd 0123 !@#$%';

type
  Int100 = 0..100;

  TGPFontParams = record
    class var BoldStr, ItalicStr, UnderlineStr, StrikeoutStr: string;
    Name: string;
    Size: integer;
    Color: TColor;
    Transparency: Int100;
    Style: TFontStyles;
    procedure AssignParams(const AName: string; const ASize: integer; const AColor: TColor; const ATransp: Int100; const AStyle: TFontStyles);
    function GetGPFontStyle: TGPFontStyle;
    function GetGPColor: TAlphaColor;
    function InfoStr(ShowColor: Boolean = True): string;
    function FontStyleStr: string;
    procedure Clear;
  end;

  TFormGPFontDialog = class(TForm)
    btnCancel: TButton;
    BtnChangeColor: TJppComboButton;
    BtnCopyColor: TJppComboButton;
    btnOK: TButton;
    BtnPasteColor: TJppComboButton;
    cbFontName: TJppComboBox;
    gbFontStyle: TGroupBox;
    chStyleBold: TJppCheckBox;
    chStyleItalic: TJppCheckBox;
    chStyleUnderline: TJppCheckBox;
    chStyleStrikeout: TJppCheckBox;
    imgExample: TImage;
    edExampleText: TJppEdit;
    lblPreview: TJppLabel;
    tmUpdateExample: TJppTimer;
    spedTransparency: TJPLazSpinEdit;
    ccbColor: TJppColorComboBox;
    spedSize: TJPLazSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure SelectFont(const FontName: string);
    procedure tmUpdateExampleTimer(Sender: TObject);
    procedure UpdateExample(Sender: TObject);
    procedure PrepareFontList;
    procedure BeginUpdateControls;
    procedure EndUpdateControls;
  private
    FExampleText: string;
    FUpdatingControls: Boolean;
    procedure SetExampleText(AValue: string);
  public
    procedure SetLang;
    procedure DrawExample;
    property ExampleText: string read FExampleText write SetExampleText;
  end;


function ExecuteGPFontDialog(var Params: TGPFontParams; MaxFontSize: integer = 50): Boolean;

function FontStylesToGPFontStyle(const Style: TFontStyles): TGPFontStyle;
function GPFontStyleToFontStyles(const Style: TGPFontStyle): TFontStyles;

var
  FormGPFontDialog: TFormGPFontDialog;
  GPFontParams: TGPFontParams;




implementation

uses
  Rozeta.FormMain;

{$R *.lfm}


{$region ' helpers '}
function FontStylesToStr(const fs: TFontStyles; BoldStr: string = 'Bold'; ItalicStr: string = 'Italic';
  UnderlineStr: string = 'Underline'; StrikeoutStr: string = 'Strikeout'): string;
const
  SEP = ',';
begin
  Result := '';
  if fsBold in fs then Result := Result + BoldStr + SEP;
  if fsItalic in fs then Result := Result + ItalicStr + SEP;
  if fsUnderline in fs then Result := Result + UnderlineStr + SEP;
  if fsStrikeOut in fs then Result := Result + StrikeoutStr;
  Result := TrimFromEnd(Result, SEP);
end;

function ExecuteGPFontDialog(var Params: TGPFontParams; MaxFontSize: integer = 50): Boolean;
var
  Form: TFormGPFontDialog;
begin
  Form := TFormGPFontDialog.Create(nil);
  try

    Form.BeginUpdateControls;
    try
      Form.SelectFont(Params.Name);
      Form.spedSize.MaxValue := MaxFontSize;
      Form.spedSize.Value := Params.Size;
      Form.ccbColor.SelectedColor := Params.Color;
      Form.chStyleBold.Checked := fsBold in Params.Style;
      Form.chStyleItalic.Checked := fsItalic in Params.Style;
      Form.chStyleUnderline.Checked := fsUnderline in Params.Style;
      Form.chStyleStrikeout.Checked := fsStrikeOut in Params.Style;
      Form.spedTransparency.Value := Params.Transparency;
    finally
      Form.EndUpdateControls;
    end;

    Form.SetLang;
    Form.DrawExample;
    Form.ShowModal;
    Result := Form.ModalResult = mrOK;

    if Result then
    begin
      Params.Name := Form.cbFontName.Text;
      Params.Size := Form.spedSize.Value;
      Params.Color := Form.ccbColor.SelectedColor;
      Params.Style := [];
      if Form.chStyleBold.Checked then Include(Params.Style, fsBold);
      if Form.chStyleItalic.Checked then Include(Params.Style, fsItalic);
      if Form.chStyleUnderline.Checked then Include(Params.Style, fsUnderline);
      if Form.chStyleStrikeout.Checked then Include(Params.Style, fsStrikeOut);
      Params.Transparency := Form.spedTransparency.Value;
    end;

  finally
    Form.Free;
  end;
end;

function FontStylesToGPFontStyle(const Style: TFontStyles): TGPFontStyle;
begin
  Result := [];
  if fsBold in Style then Include(Result, FontStyleBold);
  if fsItalic in Style then Include(Result, FontStyleItalic);
  if fsUnderline in Style then Include(Result, FontStyleUnderline);
  if fsStrikeOut in Style then Include(Result, FontStyleStrikeout);
end;

function GPFontStyleToFontStyles(const Style: TGPFontStyle): TFontStyles;
begin
  Result := [];
  if FontStyleBold in Style then Include(Result, fsBold);
  if FontStyleItalic in Style then Include(Result, fsItalic);
  if FontStyleUnderline in Style then Include(Result, fsUnderline);
  if FontStyleStrikeout in Style then Include(Result, fsStrikeout);
end;

function TranspToAlpha(const TransparencyInPercent: Byte): Byte;
begin
  if TransparencyInPercent = 0 then Result := 255
  else if TransparencyInPercent = 100 then Result := 0
  else Result := Round(255 - (TransparencyInPercent * 2.55));
end;
{$endregion helpers}



procedure TFormGPFontDialog.FormCreate(Sender: TObject);
begin
  Caption := 'Font';

  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Round(0.75 * Screen.Width);
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;

  ccbColor.ButtonChangeColor.Caption := '...';
  ccbColor.ButtonCopyColor.Caption := '';
  ccbColor.ButtonCopyColor.PngImage.Assign(FormMain.ccbBackground_Color.ButtonCopyColor.PngImage);
  ccbColor.ButtonPasteColor.Caption := '';
  ccbColor.ButtonPasteColor.PngImage.Assign(FormMain.ccbBackground_Color.ButtonPasteColor.PngImage);

  PrepareFontList;
  SelectFont('Segoe UI');
  SetLang;
  FExampleText := DEFAULT_EXAMPLE_TEXT;
  edExampleText.Text := FExampleText;
  DrawExample;
end;

procedure TFormGPFontDialog.SetLang;
begin
  if Assigned(lsMain) then
  begin
    chStyleBold.Caption := lsMain.GetString('FontStyle_Bold=Bold');
    chStyleItalic.Caption := lsMain.GetString('FontStyle_Italic=Italic');
    chStyleUnderline.Caption := lsMain.GetString('FontStyle_Underline=Underline');
    chStyleStrikeout.Caption := lsMain.GetString('FontStyle_Strikeout=Strikeout');

    ccbColor.ButtonChangeColor.Hint := lsMain.GetString('ChangeColor=Change color...');
    ccbColor.ButtonCopyColor.Hint := lsMain.GetString('CopyColor=Copy color');
    ccbColor.ButtonPasteColor.Hint := lsMain.GetString('PasteColor=Paste color');
  end;

  if Assigned(lsFontDlg) then
  begin
    Caption := lsFontDlg.GetString('Caption=Font');
    cbFontName.BoundLabel.Caption := lsFontDlg.GetString('cbFontName.BoundLabel.Caption=Font:');
    spedSize.BoundLabel.Caption := lsFontDlg.GetString('spedSize.BoundLabel.Caption=Size:');
    ccbColor.BoundLabel.Caption := lsFontDlg.GetString('ccbColor.BoundLabel.Caption=Color:');
    gbFontStyle.Caption := lsFontDlg.GetString('gbFontStyle.Caption=Style');
    edExampleText.BoundLabel.Caption := lsFontDlg.GetString('edExampleText.BoundLabel.Caption=Example text:');
    spedTransparency.BoundLabel.Caption := lsFontDlg.GetString('spedTransparency.BoundLabel.Caption=Transparency:');
    lblPreview.Caption := lsFontDlg.GetString('lblPreview.Caption=Preview:');
    btnOK.Caption := lsFontDlg.GetString('btnOK.Caption=OK');
    btnCancel.Caption := lsFontDlg.GetString('btnCancel.Caption=Cancel');
  end;

end;

procedure TFormGPFontDialog.FormResize(Sender: TObject);
begin
  UpdateExample(Self);
end;

procedure TFormGPFontDialog.PrepareFontList;
begin
  cbFontName.Items.Assign(GPFontList);
end;

procedure TFormGPFontDialog.BeginUpdateControls;
begin
  FUpdatingControls := True;
end;

procedure TFormGPFontDialog.EndUpdateControls;
begin
  FUpdatingControls := False;
end;

procedure TFormGPFontDialog.SelectFont(const FontName: string);
var
  x: integer;
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Assign(cbFontName.Items);
    sl.CaseSensitive := False;
    x := cbFontName.Items.IndexOf(Trim(FontName));
    if x >= 0 then cbFontName.ItemIndex := x
    else cbFontName.Text := 'Tahoma';
  finally
    sl.Free;
  end;
end;

procedure TFormGPFontDialog.tmUpdateExampleTimer(Sender: TObject);
begin
  DrawExample;
end;

procedure TFormGPFontDialog.UpdateExample(Sender: TObject);
begin
  if FUpdatingControls then Exit;
  FExampleText := edExampleText.Text;
  tmUpdateExample.Restart;
end;

procedure TFormGPFontDialog.DrawExample;
var
  bmp: TBitmap;
  gr: IGPGraphics;
  Brush: IGPBrush;
  Pen: IGPPen;
  xWidth, xHeight, halfW, halfH, py: Single;
  i: integer;
  RectF: TGPRectF;
  FontFamily: IGPFontFamily;
  AFont: IGPFont;
  Text, FontName: string;
  TextWidth, TextHeight: Single;
  fs: TGPFontStyle;
  clFont: TColor;
  Transp: Int100;
  FontSize: integer;
begin
  FontName := GetFontName([cbFontName.Text, 'Tahoma']);
  bmp := TBitmap.Create;
  try

    bmp.PixelFormat := pf24bit;

    xWidth := imgExample.Width;
    xHeight := imgExample.Height;
    halfW := xWidth / 2;
    halfH := xHeight / 2;

    bmp.SetSize(imgExample.Width, imgExample.Height);

    gr := TGPGraphics.Create(bmp.Canvas.Handle);
    gr.SmoothingMode := SmoothingModeHighQuality;
    gr.PixelOffsetMode := PixelOffsetModeHighQuality;


    // Background
    Brush := TGPSolidBrush.Create(GPColor(clWhite, 255));
    RectF.Initialize(0, 0, xWidth, xHeight);
    gr.FillRectangle(Brush, RectF);


    // Frame
    Pen := TGPPen.Create(GPColor(clBlack, 255), 1);
    gr.DrawRectangle(Pen, RectF);


    // Text
    Text := FExampleText;

    clFont := ccbColor.SelectedColor;
    Transp := spedTransparency.Value;
    FontSize := spedSize.Value;

    fs := [];
    if chStyleBold.Checked then Include(fs, FontStyleBold);
    if chStyleItalic.Checked then Include(fs, FontStyleItalic);
    if chStyleUnderline.Checked then Include(fs, FontStyleUnderline);
    if chStyleStrikeout.Checked then Include(fs, FontStyleStrikeout);

    try
      FontFamily := TGPFontFamily.Create(UnicodeString(FontName));
    except
      Text := 'Unsupported font!';
      clFont := clRed;
      fs := [FontStyleBold];
      Transp := 0;
      FontSize := 14;
      FontFamily := TGPFontFamily.Create(UnicodeString('Tahoma'));
    end;
    AFont := TGPFont.Create(FontFamily, FontSize, fs, TGPUnit.UnitPoint);
    Brush := TGPSolidBrush.Create(GPColor(clFont, TranspToAlpha(Transp)));

    TextHeight := GPTextHeightF(gr, UnicodeString(Text), AFont);
    py := (xHeight / 2) - (TextHeight / 2);
    gr.DrawString(UnicodeString(Text), AFont, TGPPointF.Create(10, py), Brush);


    imgExample.Picture.Assign(bmp);

  finally
    bmp.Free;
  end;
end;

procedure TFormGPFontDialog.SetExampleText(AValue: string);
begin
  if FExampleText = AValue then Exit;
  FExampleText := AValue;
  UpdateExample(Self);
end;




{ TGPFontParams }

procedure TGPFontParams.AssignParams(const AName: string; const ASize: integer; const AColor: TColor; const ATransp: Int100;
  const AStyle: TFontStyles);
begin
  Name := AName;
  Size := ASize;
  Color := AColor;
  Transparency := ATransp;
  Style := AStyle;
end;

function TGPFontParams.GetGPFontStyle: TGPFontStyle;
begin
  Result := FontStylesToGPFontStyle(Style);
end;

function TGPFontParams.GetGPColor: TAlphaColor;
begin
  Result := GPColor(Color, TranspToAlpha(Transparency));
end;

function TGPFontParams.InfoStr(ShowColor: Boolean): string;
const
  SEP = ' / ';
begin
  if not Assigned(lsMain) then Exit('');
  Result := Name + SEP + itos(Size) + ' pt';
  if ShowColor then Result := Result + SEP + 'RGB ' + ColorToRgbIntStr(Color, 3, '0', ',');
  Result := Result + SEP + lsMain.GetString('Text_FontTransparencyShort=Transp.:') + ' ' + itos(Transparency) + '%';
end;

function TGPFontParams.FontStyleStr: string;
begin
  Result := FontStylesToStr(Style, TGPFontParams.BoldStr, TGPFontParams.ItalicStr, TGPFontParams.UnderlineStr, TGPFontParams.StrikeoutStr);
end;

procedure TGPFontParams.Clear;
begin
  AssignParams('Tahoma', 10, clWindowText, 0, []);
end;

end.

