program Test;

const HJ=1+1+2;

type pchar=^char;

     TBla=(eins=1,zwei,drei);

     TBlas=set of TBla;

     PRecord=^TRecord;
     TRecord=record
      Bla:TBla;
      Blas:TBlas;
      B:byte;
     end;

{    TTestObject=OBJECT
      PUBLIC
       BlaBla:INTEGER;
       CONSTRUCTOR Create(AParameter:BYTE);
     END;}

     TTestClass2=class;

     TTestClass=class
      private
       BlaBla:integer;
       Shit:byte;
       A:TTestClass2;
      public
       constructor Create(AParameter:byte); virtual;
     end;

     TTestClass2=class(TTestClass)
      protected
       procedure Paint; message 1;
      public
       B:byte;
       constructor Create(AParameter:byte); override;
      published
       property Pixels[X,Y:integer]:integer; 
     end;

     TBlaProc=procedure(A:integer) of object; stdcall;

     TArray=array[TBla] of integer;

const Test123:TArray=(1,2,3);

resourcestring TestResourceString='Bla';

{CONSTRUCTOR TTestObject.Create(AParameter:BYTE);
BEGIN
 BlaBla:=1+AParameter;
END;}

constructor TTestClass.Create(AParameter:byte);
begin
 BlaBla:=1+AParameter;
 Shit:=Shit div 2;
 A.B:=A.B;
end;

constructor TTestClass2.Create(AParameter:byte);
begin
 inherited Create;
 Shit:=Shit*2;
end;

procedure TTestClass2.Paint;
begin
end;

procedure OneProc(A:integer);
begin
 A:=1;
end;

label DestLabel1,DestLabel2;
var A:integer=-8;
    B:integer absolute A;
    ARecord:TRecord;
    C:pchar;
begin
 A:=8;
 B:=B+4;
 C:=@A;
 OneProc(A);
 if false then begin
  A:=A+1;
 end;
 while A>8 do begin
  if A>10 then begin
// GOTO DestLabel1;
  end else begin
   goto DestLabel2;
  end;
 end;
 DestLabel1:
 A:=A-3;
 DestLabel2:
 ARecord.B:=B;
 ARecord.B:=ARecord.B+1;
 ARecord.Bla:=TBla(ARecord.B);
end.
