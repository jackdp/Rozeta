unit Rozeta.FormAbout;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  License: public domain.

  2022.06
}

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, DzHTMLText2, LazVersion, StdCtrls, ActnList, JPL.Tstr,
  Rozeta.Misc, Rozeta.AppStrings;

type
  TFormAbout = class(TForm)
    actEsc: TAction;
    Actions: TActionList;
    htt: TDzHTMLText2;
    me: TMemo;
    procedure actEscExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FixHtmText;
    procedure SetLang;
  private

  public

  end;

var
  FormAbout: TFormAbout;

implementation

{$R *.lfm}



procedure TFormAbout.FormCreate(Sender: TObject);
begin
  Caption := 'About';
  htt.Align := alClient;
  PrepareModuleStrings_About;
  FixHtmText;
end;

procedure TFormAbout.FixHtmText;
var
  s, sd: string;
begin
  if Assigned(lsAbout) then sd := lsAbout.GetString('Donation=DONATION')
  else sd := 'DONATION';
  s := me.Text;

  s := TStr.ReplaceAll(s, '%APP_NAME%', AppParams.AppName);
  s := TStr.ReplaceAll(s, '%APP_VERSION%', AppParams.AppVerStr);
  s := TStr.ReplaceAll(s, '%APP_DATE%', AppParams.AppDateStr);
  s := TStr.ReplaceAll(s, '%APP_LICENSE%', AppParams.AppLicense);
  s := TStr.ReplaceAll(s, '%AUTHOR%', AppParams.Author);
  s := TStr.ReplaceAll(s, '%URL_APP%', AppParams.UrlApplication);
  s := TStr.ReplaceAll(s, '%URL_GITHUB%', AppParams.UrlGithub);
  s := TStr.ReplaceAll(s, '%URL_DONATION%', AppParams.UrlDonation);
  s := TStr.ReplaceAll(s, '%DONATION%', '<b>' + sd + '</b>');
  s := TStr.ReplaceAll(s, '%LAZ_VER%', laz_version);
  s := TStr.ReplaceAll(s, '%FPC_VER%', {$I %FPCVERSION%});

  htt.Text := s;
end;

procedure TFormAbout.SetLang;
begin
  if not Assigned(lsAbout) then Exit;
  Caption := lsAbout.GetString('Caption=About');
  FixHtmText;
end;

procedure TFormAbout.actEscExecute(Sender: TObject);
begin
  Close;
end;

end.

