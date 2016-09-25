unit IMDataTypes;

{$mode objfpc}{$H+}

//************************************************************
//
//    Модуль IMDataTypes
//    Copyright (c) 2016  Pichugin M (e-mail: pichugin_m@mail.ru).
//    This file is part of the Assi System.
//    License for unit "AsIs"
//    ГОСТ 8.417-2002. Единицы величин
//    Версия файла: 4
//
//    Module for determine the types at information modeling
//************************************************************

interface

uses Classes, SysUtils, CsvDocument;

type

{ Data types }

   //Тип системной переменной
   TSystemType = (
       stUnknow,
       stBoolean,
       stString,
       stShortString,
       stInteger,
       stDouble,
       stCurrency,
       stExtended
   );

   //Тип данных
   TDataTypeInformation = (
       dtiUnknow,                   //- Неопределено
       dtiParameter,                //- Параметр с еденицей измерения
       dtiText,                     //- Текст
       dtiNumberInt,                //- Целое число
       dtiNumber,                   //- Число
       dtiBoolean,                  //- Да/Нет
       dtiCurrency                  //- Денежная единица
   );

   //Приставки СИ
   TMeasureUnitPrefixMeta = record
       Index      :Integer;        //
       Name       :ShortString;
       Base       :Integer;     //всегда 10
       Exponent   :Integer;     //Экспонента десятичного множителя
       PrefixLoc  :ShortString; //Локальное обозначение
       PrefixInt  :ShortString; //Международное обозначение
       CaptionLoc :ShortString; //Локальный префикс
       CaptionInt :ShortString; //Междуныродный префикс
   end;

   PMeasureUnitPrefixMeta = ^TMeasureUnitPrefixMeta;

   //Единица измерения данных
   TMeasureUnitMeta = record
       Index      :Integer;     //
       IDData     :ShortString; //GUID
       SignLoc    :ShortString; //Локальное обозначение
       SignInt    :ShortString; //Международнон обозначение
       CaptionLoc :ShortString; //Локальный префикс
       CaptionInt :ShortString; //Междуныродный префикс
   end;

   PMeasureUnitMeta = ^TMeasureUnitMeta;

   //Тип условий
   TProvisoType = (
       ptEqually,
       ptNotEqually,
       ptLess,
       ptEquallyLess,
       ptMore,
       ptEquallyMore,
       ptContains,
       ptNotContains,
       ptBegin,
       ptNotBegin,
       ptEnd,
       ptNotEnd
   );

   //Описание типа условия
   TProvisoTypeMeta = record
       Value      :TProvisoType; //ptEqualy
       Tag        :ShortString;  //=
       TypeAsText :ShortString;  //'ptEqualy'
       Caption    :ShortString;  //Равно
       Hint       :ShortString;  //Равно
   end;

   PProvisoTypeMeta = ^TProvisoTypeMeta;

   {
    Допускается включать величину измерения данных в список несколько раз
    для разных групп, при условии соблюдения идентичности остальных параметров.
   }

   //Величина измерения данных/Измеряемый параметр
   TMeasureValueMeta = record
       Index              :Integer;
       IDData             :ShortString; //GUID
       Group              :ShortString; //Локальное название группы
       Name               :ShortString; //Латинский, Слитно. Length
       Caption            :ShortString; //Лкальное название Длина
       DataType           :TDataTypeInformation; //Тип данных
       SystemType         :TSystemType; //Тип переменной в памяти
       DefaultMUnitPrefix :ShortString; //TMeasureUnitPrefixMeta
       DefaultMUnit       :ShortString; //TMeasureUnitMeta =TagInt
   end;

   PMeasureValueMeta = ^TMeasureValueMeta;

   //Переменная программы
   TDeclaredParameterMeta = record
       MeasureValue      :ShortString;  //Тип-Длина
       MeasureUnit        :ShortString;  //Единицы-Метры
       MeasureUnitPrefix  :ShortString;  //Размерность-Кило
       Name               :ShortString;  //Латинскими
       Caption            :String;
       Hint               :String;
       DefaultValue       :Variant;
       MinValue           :Double;
       MaxValue           :Double;
       MinValueActive     :Boolean;
       MaxValueActive     :Boolean;
       ListValue          :String;
       ListValueOnly      :Boolean;
   end;

   PDeclaredParameterMeta = ^TDeclaredParameterMeta;

   //Тип строения
   TBuildingType = (
       btUnknow,                    //- Неопределено
       btGround,                    //- Площадка географическая
       btBuilding,                  //- Здание/Строение
       btArea,                      //- Резервное значение
       btFloor,                     //- Этаж/Уровень/Площадка внутри здания
       btRoom,                      //- Комната/Помещение
       btLevel,                     //- Отметка
       btLogicalType,               //- Условный объект
       btSection                    //- Секция
   );

   //Тип объекта по представлению, материалу
   TRealType     = (
       rtPhysical,                  //- Физический
       rtLogical                    //- Логический
   );

   //Стадия строительства
   TLookWorkType = (
       lwtNone,                     //- Неопределено
       lwtNew,                      //- Новое
       lwtReconstruction,           //- Реконструкция
       lwtExistent                  //- Сущетвующий
   );

var
   //Описания условий
   ProvisoTypeMeta        : array of TProvisoTypeMeta;
   //Описания СИ приставок
   //https://ru.wikipedia.org/wiki/%D0%9F%D1%80%D0%B8%D1%81%D1%82%D0%B0%D0%B2%D0%BA%D0%B8_%D0%A1%D0%98
   MeasureUnitPrefixMeta  : array of TMeasureUnitPrefixMeta;
   MeasureUnitMeta        : array of TMeasureUnitMeta;
   //Величины измерения
   MeasureValueMeta      : array of TMeasureValueMeta;
   MeasureValueGroups    : TStringList;

const
   DTFILE_CSV_DELIMETER =';';
   ProvisoTypeArr     : array[0..11] of TProvisoType = (ptEqually,ptNotEqually,
                                                ptLess,ptEquallyLess,
                                                ptMore,ptEquallyMore,
                                                ptContains,ptNotContains,
                                                ptBegin,ptNotBegin,
                                                ptEnd,ptNotEnd);

   ProvisoTypeText    : array[0..11] of string = ('ptEqually','ptNotEqually',
                                                'ptLess','ptEquallyLess',
                                                'ptMore','ptEquallyMore',
                                                'ptContains','ptNotContains',
                                                'ptBegin','ptNotBegin',
                                                'ptEnd','ptNotEnd');


procedure DataTypeInit;
procedure DataTypeDeinit;
procedure MeasureValueGroupsUpdate;
procedure DataTypeExport(AFilePath:String);
procedure DataTypeImport(AFilePath:String);

function GetMeasureValueMeta(AID:ShortString):PMeasureValueMeta;
function GetMeasureUnitMeta(AID:ShortString):PMeasureUnitMeta;
function GetMeasureUnitPrefixMeta(AName:ShortString):PMeasureUnitPrefixMeta;

implementation

procedure DataTypeInit;
var
   i,k,j:integer;
begin
  MeasureValueGroups:=TStringList.Create;
  MeasureValueGroups.Sorted:=True;
  MeasureValueGroups.Duplicates:=dupIgnore;
  MeasureValueGroups.CaseSensitive:=False;

  SetLength(ProvisoTypeMeta, 12);
  i:=0;
  ProvisoTypeMeta[i].Value      :=ptEqually;
  ProvisoTypeMeta[i].Tag        :='=';
  ProvisoTypeMeta[i].TypeAsText :='ptEqually';
  ProvisoTypeMeta[i].Caption    :='Равно';
  ProvisoTypeMeta[i].Hint       :='Равно';
  inc(i);
  ProvisoTypeMeta[i].Value      :=ptNotEqually;
  ProvisoTypeMeta[i].Tag        :='<>';
  ProvisoTypeMeta[i].TypeAsText :='ptNotEqually';
  ProvisoTypeMeta[i].Caption    :='';
  ProvisoTypeMeta[i].Hint       :='';
  inc(i);
  ProvisoTypeMeta[i].Value      :=ptLess;
  ProvisoTypeMeta[i].Tag        :='<';
  ProvisoTypeMeta[i].TypeAsText :='ptLess';
  ProvisoTypeMeta[i].Caption    :='';
  ProvisoTypeMeta[i].Hint       :='';
  inc(i);
  ProvisoTypeMeta[i].Value      :=ptEquallyLess;
  ProvisoTypeMeta[i].Tag        :='<=';
  ProvisoTypeMeta[i].TypeAsText :='ptEquallyLess';
  ProvisoTypeMeta[i].Caption    :='';
  ProvisoTypeMeta[i].Hint       :='';
  inc(i);
  ProvisoTypeMeta[i].Value      :=ptMore;
  ProvisoTypeMeta[i].Tag        :='>';
  ProvisoTypeMeta[i].TypeAsText :='ptMore';
  ProvisoTypeMeta[i].Caption    :='';
  ProvisoTypeMeta[i].Hint       :='';
  inc(i);
  ProvisoTypeMeta[i].Value      :=ptEquallyMore;
  ProvisoTypeMeta[i].Tag        :='>=';
  ProvisoTypeMeta[i].TypeAsText :='ptEquallyMore';
  ProvisoTypeMeta[i].Caption    :='';
  ProvisoTypeMeta[i].Hint       :='';
  inc(i);
  ProvisoTypeMeta[i].Value      :=ptContains;
  ProvisoTypeMeta[i].Tag        :='';
  ProvisoTypeMeta[i].TypeAsText :='ptContains';
  ProvisoTypeMeta[i].Caption    :='';
  ProvisoTypeMeta[i].Hint       :='';
  inc(i);
  ProvisoTypeMeta[i].Value      :=ptNotContains;
  ProvisoTypeMeta[i].Tag        :='';
  ProvisoTypeMeta[i].TypeAsText :='ptNotContains';
  ProvisoTypeMeta[i].Caption    :='';
  ProvisoTypeMeta[i].Hint       :='';
  inc(i);
  ProvisoTypeMeta[i].Value      :=ptBegin;
  ProvisoTypeMeta[i].Tag        :='';
  ProvisoTypeMeta[i].TypeAsText :='ptBegin';
  ProvisoTypeMeta[i].Caption    :='';
  ProvisoTypeMeta[i].Hint       :='';
  inc(i);
  ProvisoTypeMeta[i].Value      :=ptNotBegin;
  ProvisoTypeMeta[i].Tag        :='';
  ProvisoTypeMeta[i].TypeAsText :='ptNotBegin';
  ProvisoTypeMeta[i].Caption    :='';
  ProvisoTypeMeta[i].Hint       :='';
  inc(i);
  ProvisoTypeMeta[i].Value      :=ptEnd;
  ProvisoTypeMeta[i].Tag        :='';
  ProvisoTypeMeta[i].TypeAsText :='ptEnd';
  ProvisoTypeMeta[i].Caption    :='';
  ProvisoTypeMeta[i].Hint       :='';
  inc(i);
  ProvisoTypeMeta[i].Value      :=ptNotEnd;
  ProvisoTypeMeta[i].Tag        :='';
  ProvisoTypeMeta[i].TypeAsText :='ptNotEnd';
  ProvisoTypeMeta[i].Caption    :='';
  ProvisoTypeMeta[i].Hint       :='';

  SetLength(MeasureUnitPrefixMeta, 21);
  i:=0;
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=0;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='';
  MeasureUnitPrefixMeta[i].PrefixInt  :='';
  MeasureUnitPrefixMeta[i].CaptionLoc :='';
  MeasureUnitPrefixMeta[i].CaptionInt :='';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=1;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='да';
  MeasureUnitPrefixMeta[i].PrefixInt  :='da';
  MeasureUnitPrefixMeta[i].CaptionLoc :='дека';
  MeasureUnitPrefixMeta[i].CaptionInt :='deca';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=2;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='г';
  MeasureUnitPrefixMeta[i].PrefixInt  :='h';
  MeasureUnitPrefixMeta[i].CaptionLoc :='гекто';
  MeasureUnitPrefixMeta[i].CaptionInt :='hecto';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=3;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='к';
  MeasureUnitPrefixMeta[i].PrefixInt  :='k';
  MeasureUnitPrefixMeta[i].CaptionLoc :='кило';
  MeasureUnitPrefixMeta[i].CaptionInt :='kilo';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=6;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='М';
  MeasureUnitPrefixMeta[i].PrefixInt  :='M';
  MeasureUnitPrefixMeta[i].CaptionLoc :='мега';
  MeasureUnitPrefixMeta[i].CaptionInt :='mega';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=9;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='Г';
  MeasureUnitPrefixMeta[i].PrefixInt  :='G';
  MeasureUnitPrefixMeta[i].CaptionLoc :='гига';
  MeasureUnitPrefixMeta[i].CaptionInt :='giga';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=12;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='Т';
  MeasureUnitPrefixMeta[i].PrefixInt  :='T';
  MeasureUnitPrefixMeta[i].CaptionLoc :='тера';
  MeasureUnitPrefixMeta[i].CaptionInt :='tera';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=15;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='П';
  MeasureUnitPrefixMeta[i].PrefixInt  :='P';
  MeasureUnitPrefixMeta[i].CaptionLoc :='пета';
  MeasureUnitPrefixMeta[i].CaptionInt :='peta';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=18;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='Э';
  MeasureUnitPrefixMeta[i].PrefixInt  :='E';
  MeasureUnitPrefixMeta[i].CaptionLoc :='экса';
  MeasureUnitPrefixMeta[i].CaptionInt :='exa';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=21;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='З';
  MeasureUnitPrefixMeta[i].PrefixInt  :='Z';
  MeasureUnitPrefixMeta[i].CaptionLoc :='зетта';
  MeasureUnitPrefixMeta[i].CaptionInt :='zetta';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=24;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='И';
  MeasureUnitPrefixMeta[i].PrefixInt  :='Y';
  MeasureUnitPrefixMeta[i].CaptionLoc :='иотта';
  MeasureUnitPrefixMeta[i].CaptionInt :='yotta';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  //---------------------------------------
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=-1;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='д';
  MeasureUnitPrefixMeta[i].PrefixInt  :='d';
  MeasureUnitPrefixMeta[i].CaptionLoc :='деци';
  MeasureUnitPrefixMeta[i].CaptionInt :='deci';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=-2;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='с';
  MeasureUnitPrefixMeta[i].PrefixInt  :='c';
  MeasureUnitPrefixMeta[i].CaptionLoc :='санти';
  MeasureUnitPrefixMeta[i].CaptionInt :='centi';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=-3;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='м';
  MeasureUnitPrefixMeta[i].PrefixInt  :='m';
  MeasureUnitPrefixMeta[i].CaptionLoc :='милли';
  MeasureUnitPrefixMeta[i].CaptionInt :='milli';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=-6;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='мк';
  MeasureUnitPrefixMeta[i].PrefixInt  :='µ';
  MeasureUnitPrefixMeta[i].CaptionLoc :='микро';
  MeasureUnitPrefixMeta[i].CaptionInt :='micro';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=-9;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='н';
  MeasureUnitPrefixMeta[i].PrefixInt  :='n';
  MeasureUnitPrefixMeta[i].CaptionLoc :='нано';
  MeasureUnitPrefixMeta[i].CaptionInt :='nano';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=-12;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='п';
  MeasureUnitPrefixMeta[i].PrefixInt  :='p';
  MeasureUnitPrefixMeta[i].CaptionLoc :='пико';
  MeasureUnitPrefixMeta[i].CaptionInt :='pico';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=-15;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='ф';
  MeasureUnitPrefixMeta[i].PrefixInt  :='f';
  MeasureUnitPrefixMeta[i].CaptionLoc :='фемто';
  MeasureUnitPrefixMeta[i].CaptionInt :='femto';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=-18;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='а';
  MeasureUnitPrefixMeta[i].PrefixInt  :='a';
  MeasureUnitPrefixMeta[i].CaptionLoc :='атто';
  MeasureUnitPrefixMeta[i].CaptionInt :='atto';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=-21;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='з';
  MeasureUnitPrefixMeta[i].PrefixInt  :='z';
  MeasureUnitPrefixMeta[i].CaptionLoc :='зепто';
  MeasureUnitPrefixMeta[i].CaptionInt :='zepto';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;
  inc(i);
  MeasureUnitPrefixMeta[i].Index      :=i;
  MeasureUnitPrefixMeta[i].Base       :=10;
  MeasureUnitPrefixMeta[i].Exponent   :=-24;
  MeasureUnitPrefixMeta[i].PrefixLoc  :='и';
  MeasureUnitPrefixMeta[i].PrefixInt  :='y';
  MeasureUnitPrefixMeta[i].CaptionLoc :='иокто';
  MeasureUnitPrefixMeta[i].CaptionInt :='yocto';
  MeasureUnitPrefixMeta[i].Name       :=MeasureUnitPrefixMeta[i].CaptionInt;

  //Формируем записи в MeasureValueMeta не связанные с MeasureUnitMeta,
  //а потом обрабатываем MeasureUnitMeta и дозаполняем массив

  SetLength(MeasureValueMeta, 10);
  i:=0;
  MeasureValueMeta[i].Index      :=i;
  MeasureValueMeta[i].IDData     :='{UNKNOW}';
  MeasureValueMeta[i].Name       :='Unknow';
  MeasureValueMeta[i].Group      :='';
  MeasureValueMeta[i].Caption    :='Неопределенный';
  MeasureValueMeta[i].DataType   :=dtiUnknow;
  MeasureValueMeta[i].SystemType :=stUnknow;
  inc(i);
  MeasureValueMeta[i].Index     :=i;
  MeasureValueMeta[i].IDData     :='{F0E72D9B-B099-4C6F-AD84-84FDF2458CBA}';
  MeasureValueMeta[i].Name       :='Boolean';
  MeasureValueMeta[i].Group      :='Общие';
  MeasureValueMeta[i].Caption    :='Логическое';
  MeasureValueMeta[i].DataType   :=dtiBoolean;
  MeasureValueMeta[i].SystemType :=stBoolean;
  inc(i);
  MeasureValueMeta[i].Index      :=i;
  MeasureValueMeta[i].IDData     :='{8C2BC2F7-23DD-4872-BC86-566EBB3DFCFC}';
  MeasureValueMeta[i].Name       :='Text';
  MeasureValueMeta[i].Group      :='Общие';
  MeasureValueMeta[i].Caption    :='Текст';
  MeasureValueMeta[i].DataType   :=dtiText;
  MeasureValueMeta[i].SystemType :=stString;
  inc(i);
  MeasureValueMeta[i].Index     :=i;
  MeasureValueMeta[i].IDData     :='{F59A9984-A4DB-44AC-B242-48ADFCA50618}';
  MeasureValueMeta[i].Name       :='Number';
  MeasureValueMeta[i].Group      :='Общие';
  MeasureValueMeta[i].Caption    :='Число';
  MeasureValueMeta[i].DataType   :=dtiNumber;
  MeasureValueMeta[i].SystemType :=stDouble;
  inc(i);
  MeasureValueMeta[i].Index     :=i;
  MeasureValueMeta[i].IDData     :='{6583EDA9-0A9E-4F04-9CE1-8AC112222323}';
  MeasureValueMeta[i].Name       :='Integer';
  MeasureValueMeta[i].Group      :='Общие';
  MeasureValueMeta[i].Caption    :='Целое число';
  MeasureValueMeta[i].DataType   :=dtiNumberInt;
  MeasureValueMeta[i].SystemType :=stInteger;
  inc(i);
  MeasureValueMeta[i].Index      :=i;
  MeasureValueMeta[i].IDData     :='{173D43BD-4A27-4343-803C-4F9E4EDAC409}';
  MeasureValueMeta[i].Name       :='Currency';
  MeasureValueMeta[i].Group      :='Общие';
  MeasureValueMeta[i].Caption    :='Валюта';
  MeasureValueMeta[i].DataType   :=dtiCurrency;
  MeasureValueMeta[i].SystemType :=stCurrency;
  inc(i);
  MeasureValueMeta[i].Index      :=i;
  MeasureValueMeta[i].IDData     :='{8970EF33-D142-429B-9C3E-81421CAF3E95}';
  MeasureValueMeta[i].Name       :='Link';
  MeasureValueMeta[i].Group      :='Общие';
  MeasureValueMeta[i].Caption    :='Ссылка';
  MeasureValueMeta[i].DataType   :=dtiText;
  MeasureValueMeta[i].SystemType :=stString;

  inc(i);
  MeasureValueMeta[i].Index      :=i;
  MeasureValueMeta[i].IDData     :='{E2C4DE72-1636-403A-A78D-5FBC11148D2A}';
  MeasureValueMeta[i].Name       :='Score';
  MeasureValueMeta[i].Group      :='Общие';
  MeasureValueMeta[i].Caption    :='Счет';
  MeasureValueMeta[i].DataType   :=dtiParameter;
  MeasureValueMeta[i].SystemType :=stDouble;

  inc(i);
  MeasureValueMeta[i].Index      :=i;
  MeasureValueMeta[i].IDData     :='{028277C4-116A-4355-9B6C-BDF50755E6DE}';
  MeasureValueMeta[i].Name       :='Percent';
  MeasureValueMeta[i].Group      :='Общие';
  MeasureValueMeta[i].Caption    :='Процент';
  MeasureValueMeta[i].DataType   :=dtiParameter;
  MeasureValueMeta[i].SystemType :=stDouble;

  inc(i);
  MeasureValueMeta[i].Index      :=i;
  MeasureValueMeta[i].IDData     :='{B7E46D21-98EB-4FDA-BFE3-6630C5140355}';
  MeasureValueMeta[i].Name       :='RelativeUnit';
  MeasureValueMeta[i].Group      :='Общие';
  MeasureValueMeta[i].Caption    :='Относительная единица';
  MeasureValueMeta[i].DataType   :=dtiParameter;
  MeasureValueMeta[i].SystemType :=stDouble;

  SetLength(MeasureUnitMeta, 5);
  i:=0;
  MeasureUnitMeta[i].Index     :=i;
  MeasureUnitMeta[i].SignLoc     :='unknow'; //Локальное обозначение
  MeasureUnitMeta[i].SignInt     :='unknow'; //Международнон обозначение
  MeasureUnitMeta[i].CaptionLoc :=''; //Локальный префикс
  MeasureUnitMeta[i].CaptionInt :=''; //Междуныродный префикс
  //Величина измерения данных
  MeasureUnitMeta[i].IDData     :='{UNKNOW}';

//Относительные и логарифмические величины
  inc(i);
  MeasureUnitMeta[i].Index     :=i;
  MeasureUnitMeta[i].SignLoc     :='Нп';
  MeasureUnitMeta[i].SignInt     :='Np';
  MeasureUnitMeta[i].CaptionLoc :='непер';
  MeasureUnitMeta[i].CaptionInt :='neper';
  //Величина измерения данных
  MeasureUnitMeta[i].IDData     :='{B7E46D21-98EB-4FDA-BFE3-6630C5140355}';
  //'Логарифмы, Непер';
  inc(i);
  MeasureUnitMeta[i].Index     :=i;
  MeasureUnitMeta[i].SignLoc     :='Б';
  MeasureUnitMeta[i].SignInt     :='B';
  MeasureUnitMeta[i].CaptionLoc :='бел';
  MeasureUnitMeta[i].CaptionInt :='bel';
  //Величина измерения данных
  MeasureUnitMeta[i].IDData     :='{B7E46D21-98EB-4FDA-BFE3-6630C5140355}';
  //'Логарифмы, Бел';
  inc(i);
  MeasureUnitMeta[i].Index     :=i;
  MeasureUnitMeta[i].SignLoc     :='%';
  MeasureUnitMeta[i].SignInt     :='%';
  MeasureUnitMeta[i].CaptionLoc :='процент';
  MeasureUnitMeta[i].CaptionInt :='percent';
  //Величина измерения данных
  MeasureUnitMeta[i].IDData     :='{028277C4-116A-4355-9B6C-BDF50755E6DE}';
  //'Процент';
  inc(i);
  MeasureUnitMeta[i].Index     :=i;
  MeasureUnitMeta[i].SignLoc     :='ед';
  MeasureUnitMeta[i].SignInt     :='u';
  MeasureUnitMeta[i].CaptionLoc :='единица';
  MeasureUnitMeta[i].CaptionInt :='unit';
  //Величина измерения данных
  MeasureUnitMeta[i].IDData     :='{E2C4DE72-1636-403A-A78D-5FBC11148D2A}';
  //'Счет';
end;

procedure DataTypeDeinit;
begin
  MeasureValueGroups.Free;
  SetLength(ProvisoTypeMeta, 0);
  SetLength(MeasureUnitPrefixMeta, 0);
  SetLength(MeasureValueMeta, 0);
  SetLength(MeasureUnitMeta, 0);
end;

procedure MeasureValueGroupsUpdate;
var
   i:integer;
begin
   MeasureValueGroups.Clear;
   for i:=0 to high(MeasureValueMeta)do
   begin
       if MeasureValueMeta[i].Group<>'' then
       MeasureValueGroups.Add(MeasureValueMeta[i].Group);
   end;
end;

procedure DataTypeExport(AFilePath:String);
var
   Csv :TCsvDocument;
   i   :integer;
begin
   Csv           :=TCsvDocument.Create;
   Csv.Delimiter :=DTFILE_CSV_DELIMETER;
   for i:=0 to high(MeasureUnitMeta)do
   begin
       Csv.AddCell(i,IntToStr(MeasureUnitMeta[i].Index));
       Csv.AddCell(i,MeasureUnitMeta[i].IDData);
       Csv.AddCell(i,MeasureUnitMeta[i].SignInt);
       Csv.AddCell(i,MeasureUnitMeta[i].SignLoc);
       Csv.AddCell(i,MeasureUnitMeta[i].CaptionInt);
       Csv.AddCell(i,MeasureUnitMeta[i].CaptionLoc);
   end;
   Csv.SaveToFile(IncludeTrailingPathDelimiter(AFilePath)+'mum.csv');
   Csv.Free;

   Csv           :=TCsvDocument.Create;
   Csv.Delimiter :=DTFILE_CSV_DELIMETER;
   for i:=0 to high(MeasureValueMeta)do
   begin
       Csv.AddCell(i,IntToStr(MeasureValueMeta[i].Index));
       Csv.AddCell(i,MeasureValueMeta[i].IDData);
       Csv.AddCell(i,MeasureValueMeta[i].Name);
       Csv.AddCell(i,MeasureValueMeta[i].Caption);
   end;
   Csv.SaveToFile(IncludeTrailingPathDelimiter(AFilePath)+'mvm.csv');
   Csv.Free;
end;

procedure DataTypeImport(AFilePath:String);
var
   Csv     :TCsvDocument;
   i,k,j,y :integer;
   tmpSStr1,
   tmpSStr2:ShortString;
   bCheck,
   bIns    :boolean;
begin
   Csv           :=TCsvDocument.Create;
   Csv.Delimiter :=DTFILE_CSV_DELIMETER;
   Csv.LoadFromFile(IncludeTrailingPathDelimiter(AFilePath)+'mum.csv');
   k             :=Length(MeasureUnitMeta);
   j             :=k+Csv.RowCount;
   if (Csv.ColCount[0]>=6) then
   begin
     SetLength(MeasureUnitMeta,j);
     j:=k-1;
     bIns:=False;
     for i:=0 to Csv.RowCount-1 do
     begin
        tmpSStr1 :=Csv.Cells[2,i];
        tmpSStr2 :=Csv.Cells[4,i];
        bCheck   :=True;
        for y:=0 to high(MeasureUnitMeta) do
        begin
           if (ShortCompareText(MeasureUnitMeta[y].SignInt, tmpSStr1)=0)
           and(ShortCompareText(MeasureUnitMeta[y].CaptionInt, tmpSStr2)=0)then
           begin
              bCheck:=False;
              break;
           end;
        end;
        if bCheck then
        begin
          bIns:=True;
          inc(j);
          MeasureUnitMeta[j].Index      :=j;
          MeasureUnitMeta[j].IDData     :=Csv.Cells[1,i];
          MeasureUnitMeta[j].SignInt    :=Csv.Cells[2,i];
          MeasureUnitMeta[j].SignLoc    :=Csv.Cells[3,i];
          MeasureUnitMeta[j].CaptionInt :=Csv.Cells[4,i];
          MeasureUnitMeta[j].CaptionLoc :=Csv.Cells[5,i];
        end;
     end;
     if bIns then
     begin
       inc(j);
       SetLength(MeasureUnitMeta,j);
     end;
   end;
   Csv.Free;

   Csv              :=TCsvDocument.Create;
   Csv.Delimiter    :=DTFILE_CSV_DELIMETER;
   Csv.LoadFromFile(IncludeTrailingPathDelimiter(AFilePath)+'mvm.csv');
   k                :=Length(MeasureValueMeta);
   j                :=k+Csv.RowCount;
   if (Csv.ColCount[0]>=5) then
   begin
     SetLength(MeasureValueMeta,j);
     j:=k-1;
     bIns:=False;
     for i:=0 to Csv.RowCount-1 do
     begin
          tmpSStr1 :=Csv.Cells[1,i];
          tmpSStr2 :=Csv.Cells[4,i];
          bCheck   :=True;
          for y:=0 to high(MeasureValueMeta) do
          begin
             if (ShortCompareText(MeasureValueMeta[y].IDData, tmpSStr1)=0)and
                (ShortCompareText(MeasureValueMeta[y].Group, tmpSStr2)=0)then
             begin
                bCheck:=False;
                break;
             end;
          end;
          if bCheck then
          begin
            bIns:=True;
            inc(j);
            MeasureValueMeta[j].Index      :=j;
            MeasureValueMeta[j].IDData     :=Csv.Cells[1,i];
            MeasureValueMeta[j].Name       :=Csv.Cells[2,i];;
            MeasureValueMeta[j].Caption    :=Csv.Cells[3,i];
            MeasureValueMeta[j].Group      :=Csv.Cells[4,i];
            MeasureValueMeta[j].DataType   :=dtiParameter;
            MeasureValueMeta[j].SystemType :=stDouble;
          end;
     end;
     if bIns then
     begin
       inc(j);
       SetLength(MeasureValueMeta,j);
     end;
   end;
   Csv.Free;
end;

function GetMeasureValueMeta(AID: ShortString): PMeasureValueMeta;
var
   i:integer;
begin
   Result:=nil;
   for i:=0 to high(MeasureValueMeta)do
   begin
       if ShortCompareText(MeasureValueMeta[i].IDData,AID)=0 then
       begin
          Result:=Addr(MeasureValueMeta[i]);
          break;
       end;
   end;
end;

function GetMeasureUnitMeta(AID: ShortString): PMeasureUnitMeta;
var
   i:integer;
begin
   Result:=nil;
   for i:=0 to high(MeasureUnitMeta)do
   begin
       if ShortCompareText(MeasureUnitMeta[i].IDData,AID)=0 then
       begin
          Result:=Addr(MeasureUnitMeta[i]);
          break;
       end;
   end;
end;

function GetMeasureUnitPrefixMeta(AName: ShortString): PMeasureUnitPrefixMeta;
var
   i:integer;
begin
   Result:=nil;
   for i:=0 to high(MeasureUnitPrefixMeta)do
   begin
       if ShortCompareText(MeasureUnitPrefixMeta[i].Name,AName)=0 then
       begin
          Result:=Addr(MeasureUnitPrefixMeta[i]);
          break;
       end;
   end;
end;

end.

