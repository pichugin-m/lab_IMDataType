unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ComCtrls, IMDataTypes;

type

  { TFParameterEdit }

  TFParameterEdit = class(TForm)
    btnAdd: TButton;
    btnDel: TButton;
    btnEditTables: TButton;
    btnRefresh: TButton;
    cbGroup: TComboBox;
    cbListValueOnly: TCheckBox;
    cbMaxActive: TCheckBox;
    cbMeasureValue: TComboBox;
    cbMeasureUnit: TComboBox;
    cbMeasureUnitPrefix: TComboBox;
    cbMinActive: TCheckBox;
    edtCaption: TEdit;
    edtDefaultValue: TEdit;
    edtDescription: TEdit;
    edtMeasureUnit: TEdit;
    edtName: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lvDeclParam: TListView;
    mListValue: TMemo;
    PageControl1: TPageControl;
    seMaxValue: TFloatSpinEdit;
    seMinValue: TFloatSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnAddClick(Sender: TObject);
    procedure btnDelClick(Sender: TObject);
    procedure btnEditTablesClick(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure cbGroupChange(Sender: TObject);
    procedure cbMeasureValueChange(Sender: TObject);
    procedure cbMeasureUnitChange(Sender: TObject);
    procedure cbMeasureUnitPrefixChange(Sender: TObject);
    procedure edtNameKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
    procedure lvDeclParamSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  private
    procedure RefreshMeasureUnit;
    procedure RefreshMeasureValue;
    procedure RefreshMeasureUnitFinish;
    { private declarations }
  public
    { public declarations }
  end;

var
  FParameterEdit       : TFParameterEdit;
  DeclaredParameterArr : array of TDeclaredParameterMeta;

implementation

{$R *.lfm}

{ TFParameterEdit }

procedure TFParameterEdit.FormCreate(Sender: TObject);
var
  i:integer;
begin
  DataTypeInit;
  //DataTypeExport('.');
  DataTypeImport('.');
  MeasureValueGroupsUpdate;

  for i:=0 to high(MeasureUnitPrefixMeta) do
  begin
      if MeasureUnitPrefixMeta[i].PrefixInt<>'' then
        cbMeasureUnitPrefix.Items.AddObject(format('%s (%d^%d)',[MeasureUnitPrefixMeta[i].CaptionLoc, MeasureUnitPrefixMeta[i].Base, MeasureUnitPrefixMeta[i].Exponent]),TObject(i))
      else
        cbMeasureUnitPrefix.Items.AddObject('Нет',TObject(i));
  end;

  cbGroup.Items.Text:=MeasureValueGroups.Text;
  cbGroup.ItemIndex:=0;

  RefreshMeasureValue;
end;

procedure TFParameterEdit.lvDeclParamSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  i,j:integer;
  mvm:PMeasureValueMeta;
  mupm:PMeasureUnitPrefixMeta;
  mum:PMeasureUnitMeta;
  dpm:PDeclaredParameterMeta;
begin
  i:=lvDeclParam.ItemIndex;
  if i=-1 then exit;
  dpm:=PDeclaredParameterMeta(lvDeclParam.Items.Item[i].Data);
  //k :=StrToInt(lvDeclParam.Items.Item[i].SubItems.Strings[3]);

  edtCaption.Text           :=dpm^.Caption;
  edtName.Text              :=dpm^.Name;
  edtDescription.Text       :=dpm^.Hint;
  edtDefaultValue.Text      :=dpm^.DefaultValue;
  seMaxValue.Value          :=dpm^.MaxValue;
  seMinValue.Value          :=dpm^.MinValue;
  cbListValueOnly.Checked   :=dpm^.ListValueOnly;
  cbMaxActive.Checked       :=dpm^.MaxValueActive;
  cbMinActive.Checked       :=dpm^.MinValueActive;
  mListValue.Lines.Text     :=dpm^.ListValue;

  mvm  :=GetMeasureValueMeta(dpm^.MeasureValue);
  j    :=cbGroup.Items.IndexOf(mvm^.Group);
  cbGroup.ItemIndex:=j;
  RefreshMeasureValue;

  j    :=cbMeasureValue.Items.IndexOfObject(TObject(mvm^.Index));
  cbMeasureValue.ItemIndex:=j;
  RefreshMeasureUnit;

  mupm :=GetMeasureUnitPrefixMeta(dpm^.MeasureUnitPrefix);
  j    :=cbMeasureUnitPrefix.Items.IndexOfObject(TObject(mupm^.Index));
  cbMeasureUnitPrefix.ItemIndex:=j;

  mum  :=GetMeasureUnitMeta(dpm^.MeasureValue);
  if cbMeasureUnit.Items.Count>0 then
  begin
    j    :=cbMeasureUnit.Items.IndexOfObject(TObject(mum^.Index));
    cbMeasureUnit.ItemIndex:=j;
  end;
  RefreshMeasureUnitFinish;
end;

procedure TFParameterEdit.cbMeasureValueChange(Sender: TObject);
begin
  RefreshMeasureUnit;
end;

procedure TFParameterEdit.cbGroupChange(Sender: TObject);
begin
  RefreshMeasureValue;
end;

procedure TFParameterEdit.btnRefreshClick(Sender: TObject);
var
  i:integer;
  Item:TListItem;
begin
  lvDeclParam.BeginUpdate;
  lvDeclParam.Clear;
  for i:=0 to high(DeclaredParameterArr) do
  begin
      Item:=lvDeclParam.Items.Add;
      Item.SubItems.Add(DeclaredParameterArr[i].Caption);
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.SubItems.Add('');
      Item.Data:=Addr(DeclaredParameterArr[i]);
      Item.Caption:=DeclaredParameterArr[i].Name;
  end;
  lvDeclParam.EndUpdate;
end;

procedure TFParameterEdit.btnAddClick(Sender: TObject);
var
  l,k:integer;
begin
  l:=-1;
  for k:=0 to high(DeclaredParameterArr) do
  begin
    if CompareText(DeclaredParameterArr[k].Name, edtName.Text)=0 then
    begin
      l:=1;
      Application.MessageBox('Параметр существует',PChar(Caption),MB_ICONINFORMATION);
      break;
    end;
  end;
  if l>-1 then exit;

  l:=Length(DeclaredParameterArr);
  inc(l);
  SetLength(DeclaredParameterArr,l);
  dec(l);
  DeclaredParameterArr[l].Caption           :=edtCaption.Text;
  DeclaredParameterArr[l].Name              :=edtName.Text;
  DeclaredParameterArr[l].Hint              :=edtDescription.Text;
  DeclaredParameterArr[l].DefaultValue      :=edtDefaultValue.Text;
  DeclaredParameterArr[l].MaxValue          :=seMaxValue.Value;
  DeclaredParameterArr[l].MinValue          :=seMinValue.Value;
  DeclaredParameterArr[l].ListValueOnly     :=cbListValueOnly.Checked;
  DeclaredParameterArr[l].MaxValueActive    :=cbMaxActive.Checked;
  DeclaredParameterArr[l].MinValueActive    :=cbMinActive.Checked;
  DeclaredParameterArr[l].ListValue         :=mListValue.Lines.Text;

  k:=Integer(cbMeasureValue.Items.Objects[cbMeasureValue.ItemIndex]);
  DeclaredParameterArr[l].MeasureValue     :=MeasureValueMeta[k].IDData;
  if cbMeasureUnit.ItemIndex=-1 then
      k:=0
  else
      k:=Integer(cbMeasureUnit.Items.Objects[cbMeasureUnit.ItemIndex]);
  DeclaredParameterArr[l].MeasureUnit      :=MeasureUnitMeta[k].IDData;
  if cbMeasureUnitPrefix.ItemIndex=-1 then
     k:=0
  else
     k:=Integer(cbMeasureUnitPrefix.Items.Objects[cbMeasureUnitPrefix.ItemIndex]);
  DeclaredParameterArr[l].MeasureUnitPrefix :=MeasureUnitPrefixMeta[k].Name;

  btnRefresh.Click;
end;

procedure TFParameterEdit.btnDelClick(Sender: TObject);
var
  i,l,k,j:integer;
  dpm:PDeclaredParameterMeta;
begin
  i:=lvDeclParam.ItemIndex;
  l:=Length(DeclaredParameterArr);
  j:=l-1;
  if i=-1 then exit;
  dpm:=PDeclaredParameterMeta(lvDeclParam.Items.Item[i].Data);
  for k:=0 to high(DeclaredParameterArr) do
  begin
    if CompareText(DeclaredParameterArr[k].Name, dpm^.Name)=0 then
    begin
      DeclaredParameterArr[k]:=DeclaredParameterArr[j];
      SetLength(DeclaredParameterArr,j);
      break;
    end;
  end;
  //lvDeclParam.Items.Delete(i);
  btnRefresh.Click;
end;

procedure TFParameterEdit.btnEditTablesClick(Sender: TObject);
var
  l,k:integer;
begin
  l:=Length(DeclaredParameterArr);
  inc(l);
  SetLength(DeclaredParameterArr,l);
  dec(l);
  DeclaredParameterArr[l].Caption           :=edtCaption.Text;
  DeclaredParameterArr[l].Name              :=edtName.Text;
  DeclaredParameterArr[l].Hint              :=edtDescription.Text;
  DeclaredParameterArr[l].DefaultValue      :=edtDefaultValue.Text;
  DeclaredParameterArr[l].MaxValue          :=seMaxValue.Value;
  DeclaredParameterArr[l].MinValue          :=seMinValue.Value;
  DeclaredParameterArr[l].ListValueOnly     :=cbListValueOnly.Checked;
  DeclaredParameterArr[l].MaxValueActive    :=cbMaxActive.Checked;
  DeclaredParameterArr[l].MinValueActive    :=cbMinActive.Checked;
  DeclaredParameterArr[l].ListValue         :=mListValue.Lines.Text;

  k:=Integer(cbMeasureValue.Items.Objects[cbMeasureValue.ItemIndex]);
  DeclaredParameterArr[l].MeasureValue     :=MeasureValueMeta[k].IDData;
  k:=Integer(cbMeasureUnit.Items.Objects[cbMeasureUnit.ItemIndex]);
  DeclaredParameterArr[l].MeasureUnit       :=MeasureUnitMeta[k].IDData;
  k:=Integer(cbMeasureUnitPrefix.Items.Objects[cbMeasureUnitPrefix.ItemIndex]);
  DeclaredParameterArr[l].MeasureUnitPrefix :=MeasureUnitPrefixMeta[k].Name;

  btnRefresh.Click;
end;

procedure TFParameterEdit.cbMeasureUnitChange(Sender: TObject);
begin
  RefreshMeasureUnitFinish;
end;

procedure TFParameterEdit.cbMeasureUnitPrefixChange(Sender: TObject);
begin
  RefreshMeasureUnitFinish;
end;

procedure TFParameterEdit.edtNameKeyPress(Sender: TObject; var Key: char);
begin
  if not(key in ['a'..'z','A'..'Z','0'..'9','_']) then
    key:=#0;
end;

procedure TFParameterEdit.RefreshMeasureUnit;
var
  k,i:integer;
begin
  cbMeasureUnit.Items.BeginUpdate;
  cbMeasureUnit.Items.Clear;
  k:=Integer(cbMeasureValue.Items.Objects[cbMeasureValue.ItemIndex]);
    if k>-1 then
    begin
      for i:=0 to high(MeasureUnitMeta) do
      begin
          if CompareText(MeasureUnitMeta[i].IDData, MeasureValueMeta[k].IDData)=0 then
          cbMeasureUnit.Items.AddObject(format('%s',[MeasureUnitMeta[i].SignLoc]), TObject(i));
      end;
    end;
  cbMeasureUnit.Items.EndUpdate;
  cbMeasureUnit.ItemIndex:=0;
  cbMeasureUnitPrefix.ItemIndex:=0;
  RefreshMeasureUnitFinish;
end;

procedure TFParameterEdit.RefreshMeasureValue;
var
  i:integer;
begin
  cbMeasureValue.Items.BeginUpdate;
  cbMeasureValue.Items.Clear;
  for i:=0 to high(MeasureValueMeta) do
  begin
      if CompareText(cbGroup.Items.Strings[cbGroup.ItemIndex],MeasureValueMeta[i].Group)=0 then
      cbMeasureValue.Items.AddObject(format('%s',[MeasureValueMeta[i].Caption]),TObject(i));
  end;
  cbMeasureValue.Items.EndUpdate;
  cbMeasureValue.ItemIndex:=0;
  RefreshMeasureUnit;
end;

procedure TFParameterEdit.RefreshMeasureUnitFinish;
begin
  if (cbMeasureUnit.ItemIndex>-1)and(cbMeasureUnitPrefix.ItemIndex>-1) then
  edtMeasureUnit.Text:=MeasureUnitPrefixMeta[cbMeasureUnitPrefix.ItemIndex].PrefixLoc+MeasureUnitMeta[Integer(cbMeasureUnit.Items.Objects[cbMeasureUnit.ItemIndex])].SignLoc;

end;

end.

