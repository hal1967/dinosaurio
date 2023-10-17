unit umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  uplaysound;

type

  { TForm1 }

  TForm1 = class(TForm)
    LblPaisaje: TLabel;
    LblPaisaje1: TLabel;
    Img2Pie: TImage;
    ImgCactus: TImage;
    ImgPieDer: TImage;
    ImgPieIzq: TImage;
    LblScore: TStaticText;
    playsound1: Tplaysound;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure Timer1Timer(Sender: TObject);
  private
    Img :  TImage;
    posicion, puntos, Salto : integer;
    ind_pie : integer;

  const
    paso = 25;  // La imagen y salto se mueven a 25 pixeles por cada ciclo.
    base = 200; // Esto es el tope de la imagen del dinosaurio


    procedure PasoIzq;
    procedure puntaje;
    procedure EstablecerImagenDinosaurio;
    procedure ElCactusViene;
  public

  end;

var
  Form1: TForm1;


implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormKeyPress(Sender: TObject; var Key: char);
begin
  // El formulario principal debe tener la propieda KeyPreview en true
  // basicamente por si ahy algun otro elemento en el formulario que use el teclado
  // tenga menso prioridad.

  // Salto, es la unica opcion que tenemos actuamente
  // por ello la barra espaciadora.
  // para una version futura agacharse
  if key = ' ' then
    Salto := 7;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Tenemos tres imágenes del dinosaurio
  // 2 pies sobre el suelo
  // pie izquiedo arriba
  // pie derecho abajo
  // asi que caminar es intercambiar estas imágenes en orden

  // una version optima de esto seria tener los pies separados en una
  // imagen distinta al cuerpo. Así casi siempre sería el cuerpo la misma imagen
  // excepto para los saltos.
  Img2Pie.Top   := base;   Img2Pie.Visible:= true;      Img2Pie.Left:= 180;
  ImgPieDer.Top := base;   ImgPieDer.Visible:= false;   ImgPieDer.Left:= 180;
  ImgPieIzq.Top := base;   ImgPieIzq.Visible:= false;   ImgPieIzq.Left:= 180;

  Img := Img2Pie;     // La primera imagen es en 2 pies
  ind_pie := 0;       // este contador establece cual imagen vemos
                      // 0: dos pies abajo
                      // 1: pie izquierdo arriba
                      // 2: dos pies abajo
                      // 3: pie derecho arriba
                      // y volvemos a cero.

  // Nos aseguramos que el fondo quede... en el fondo.
  LblPaisaje.SendToBack;
  LblPaisaje1.SendToBack;
  LblPaisaje.Top:= 216;
  LblPaisaje1.Top:= 216;



  ImgCactus.Top:= 250;
end;

// Este timer está ejecutando 10 veces por segundos (  Intervalo de 100 milisegundos)
// a medida que se avanza en puntaje se coloca el tiempo más corto

// Cada vuelta o ciclo, se mueve el fondo
// se actualiza score
// se mueve el disnosaurio (solo las patas)
// se verifica cactus, choque, etc
procedure TForm1.Timer1Timer(Sender: TObject);
begin
  PasoIzq;
  puntaje;
  EstablecerImagenDinosaurio;
  ElCactusViene;
end;

procedure TForm1.PasoIzq;
begin
  {
     Tenemos dos TLabel para el paisaje, LblPaisaje y LblPaisaje1
     basicamente una serie de "_" y "-" puestos juntos
     Ambas etiquetas son iguales y mas anchas que el formulario

       ____-_____--___    y    ____-_____--___

     la poscion de LblPaisaje es LblPaisaje.With + LblPaisaje.Left + 1, es decir
     donde termina LblPaisaje comienza LblPaisaje.

     Cuando ya no se ve LblPaisaje se reinicia a posicion 0

  }
  posicion := posicion - paso;   // Un paso a la izquierda
  // si posicion = -LblPaisaje.Width siginifica que ya no se ve
  // LblPaisaje.Width y reiniciamos, como las dos etiquetas
  // son iguales no se nota el cambio

  if posicion <= -LblPaisaje.Width then posicion := 0;

  LblPaisaje.Left:= posicion;
  LblPaisaje1.Left:= LblPaisaje.Left + LblPaisaje.Width + 1;
end;

procedure TForm1.puntaje;
begin
  // Estamos contando el score un punto por ciclo
  inc(puntos);

  // Cuando hay choque, la etiqueta de score es roja, luego de un
  // rato (20 puntos) el colocr por defecto
  if Puntos = 20 then
    LblScore.Font.Color:= clDefault
  else if puntos = 250 then  // Acelerar es hacer los intervalos mas cortos
    Timer1.Interval:= 75
  else if puntos = 500 then
    Timer1.Interval:= 50
  else if puntos = 1000 then
    Timer1.Interval:= 25;

  LblScore.Caption := puntos.ToString ;

end;

procedure TForm1.EstablecerImagenDinosaurio;
begin

    // ind_pie para llevar la cuenta, que pie va arriba o abajo
    ind_pie:= (ind_pie + 1) mod 4;  // base, sube izq,  base, sube der

    // Hay varios enfoque para hacer esto
    //  1- Tener un array img[0..3] donde cada indice corresponde a una imagen
    //  2- Apuntar a la imagen que se va a usar.
    //  3- Usar imagelist y desde alli dibujar (requiere mas conociemiento de VCL)
    //  4. Usar una lista, incluso una propiedad (opcion interesante)
    //
    //  Nos quedamos con la segunda opción por ahora
    // img es la imagen que se está por mostrar


    // Como vamos a cambiar de imagen, la actual se hace invisible.

    img.Visible:= false;
    // Case es rápido, usa run array sería mas eficiente, pero ya no estamos
    // en tiempo del Atari 2600
    Case ind_pie of // base, sube izq,  base, sube der
    0: img := Img2Pie;
    1: img := ImgPieIzq;
    2: img := Img2Pie;
    3: img := ImgPieDer;
    end;

    // El brinco o salto no se hace en un solo movimeinto
    // sino en 6 movimientos.

    // Una falla conceptual que mientras mas rápido
    // va el juego, más rápido es el salto,
    // lo cual tendría sentido si la gravedad o el peso
    // cambia. Por lo pronto ignoraremos la física
    // pero para resolverlo deberiamos llevar algún itpo de contador.

    // Paso no es mas que la distnacia que subimos en cada ciclo,  como
    // 25 pixeles pero seberiamos ajustarlo o porder escalarlo
    // po si alguien quiere correr el juego en un tv de 4K y en
    // dispositivo de test de embarazo.

    if Salto>0 then
      begin
        Salto := Salto - 1;

        // Podemos  hacer algunos trucos, pero la idea es que sea
        // entendible, 5 sería el inicio del salto, luego 4 y en 3 el tope.
        // bajamos hasta el 0

        Case Salto of
        6: Img.Top := base - 2*paso;
        5: Img.Top := base - 3*paso;
        4: Img.Top := base - 4*paso;
        3: Img.Top := base - 4*paso;
        2: Img.Top := base - 3*paso;
        1: Img.Top := base - 2*paso;
        0: Img.Top := base;
        end;

      end
    else
      Img.Top := base; // No quedamos en el piso.

    img.Visible:= true; // Ya la imagen esta en posición y se puede mostrar
end;

procedure TForm1.ElCactusViene;
begin
    // De vez en cuando se presenta un malvado cactus
    // debe ser dibujado con todo lo demás, en una posición
    // fija del paisaje
    // así que con el paisaje se mueve un paso a la vez

    if ImgCactus.Visible then
      begin
        ImgCactus.Left:=  ImgCactus.Left - paso;

        // Choque
        if (  ImgCactus.Left                    <=  (Img.Left + (2*Img.Width div 3))) AND
           ( (ImgCactus.Left + ImgCactus.Width) >=  (Img.Left )) AND
           ( (Img.Top + Img.Height) >= ImgCactus.Top)  then
             begin
               LblScore.Caption:= 'Choque';
               puntos := 0;
               Timer1.Interval:= 100;
               LblScore.Font.Color:= clRed;

               playsound1.Execute;
             end;


        if ImgCactus.Left< -ImgCactus.Width then
          ImgCactus.Visible := false;

      end
    else // Si no hay cactus, vemos si aparecemos uno.
     begin
       if Random(40) = 17 then // Una vez de cada 40, al azar.
         begin
           // Self es esta clase, Form1, referirlo como self u omintirlo es
           // casi lo mismo, pero la idea mantener el codigo portable.
           ImgCactus.Left:= Self.Width; // Al final de la pantalla, de hecho afuera
           ImgCactus.Visible := true;  // Se hace visible (bueno em la siguiente vuelta)
         end;
     end;
end;

end.

