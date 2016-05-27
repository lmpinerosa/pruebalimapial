IMPORT os

SCHEMA operativa

GLOBALS
  DEFINE
    g_usuario LIKE top_usuarios.usr_userid,
    g_programa LIKE top_programas.opc_rout,
    g_perfil   LIKE top_perfiles.per_codperfil,
    g_sucursal LIKE top_sucursal.suc_cod,
    g_nom_suc  LIKE top_sucursal.suc_descripcion,
    g_accion   LIKE top_opcxprog.oxp_name,
    g_pais  LIKE top_ciudad.pai_idpais,
    gr_vehiculo          RECORD 
    veh_placa            LIKE    top_vehiculo.veh_placa,
    ciu_idciudad         LIKE    top_ciudad.ciu_idciudad,
    ciu_nomciudad        LIKE    top_ciudad.ciu_nomciudad, 
    tip_idtipo           LIKE    top_vehiculo.tip_idtipo,   
    tip_nomtipo          LIKE    top_tiposervicio.tip_nomtipo,
    lin_idlinea          LIKE    top_vehiculo.lin_idlinea, 
    mar_nommarca         LIKE    top_marca.mar_nommarca,
    lin_nomlinea         LIKE    top_linea.lin_nomlinea, 
    cla_idclaseveh       LIKE    top_vehiculo.cla_idclaseveh,
    cla_nomclase         LIKE    top_clase_vehiculo.cla_nomclase,
    car_idcarroce        LIKE    top_vehiculo.car_idcarroce,
    car_nomcarroce       LIKE    top_carroceria.car_nomcarroce,
    col_idcolor          LIKE    top_vehiculo.col_idcolor,
    col_nomcolor         LIKE    top_color.col_nomcolor, 
    cap_idcapaci         LIKE    top_vehiculo.cap_idcapaci, 
    cap_nomcapaci        LIKE    top_capacidad.cap_nomcapaci, 
    com_idcombus         LIKE    top_vehiculo.com_idcombus, 
    com_nomcombus        LIKE    top_combustible.com_nomcombus, 
    veh_motor            LIKE    top_vehiculo.veh_motor, 
    veh_serie            LIKE    top_vehiculo.veh_serie, 
    veh_modelo           LIKE    top_vehiculo.veh_modelo, 
    veh_cilindraje       LIKE    top_vehiculo.veh_cilindraje, 
    veh_orden            LIKE    top_vehiculo.veh_orden,
    emp_idempresa        LIKE    top_vehiculo.emp_idempresa,
    emp_nomempresa       LIKE    top_empresa.emp_nomempresa,
    sit_codsitio         LIKE    top_vehiculo.sit_codsitio,
    sit_nomsitio         LIKE    top_sitio.sit_nomsitio,
    veh_numinterno       LIKE    top_vehiculo.veh_numinterno,
    est_id               LIKE    top_vehiculo.est_id,
    est_descripcion      LIKE    top_estado.est_descripcion,
    veh_fechafilia       LIKE    top_vehiculo.veh_fechafilia,
    veh_fechadesa        LIKE    top_vehiculo.veh_fechadesa,
    per_tipoidenti       LIKE    top_persona.per_tipoidenti,
    per_numidenti        LIKE    top_persona.per_numidenti,
    per_nombre           LIKE    top_persona.per_nombre,
    veh_codigo           LIKE    top_vehiculo.veh_codigo
    END RECORD,
    g_existe_vehiculo    BOOLEAN,
    g_existe_orden       BOOLEAN,
    g_existe_dato        BOOLEAN,
    gr_persona  RECORD  
       per_numidenti          LIKE  top_persona.per_numidenti,
       per_nombre             LIKE  top_persona.per_nombre,
       tip_nomtipoper         LIKE  top_tipopersona.tip_nomtipoper,
       est_descripcion        LIKE  top_estado.est_descripcion,
       per_fechavincula       LIKE  top_persovehiculo.per_fechavincula
    END RECORD,
    arr_persona DYNAMIC ARRAY OF RECORD  
       cedula          LIKE  top_persona.per_numidenti,
       nombre          LIKE  top_persona.per_nombre,
       tipo            LIKE  top_tipopersona.tip_nomtipoper,
       estado          LIKE  top_estado.est_descripcion,
       per_fechavincula       LIKE  top_persovehiculo.per_fechavincula
    END RECORD
DEFINE imagelinea STRING,
 g_company LIKE top_company.cmp_ident,
 gr_sucursal RECORD LIKE top_sucursal.*,
 windows ui.Window,
 forma ui.Form
   
END GLOBALS

MAIN
DEFINE cuantos INTEGER
DEFINE nombre STRING
CONNECT TO 'operativa'

IF arg_val(1)="M" THEN
  ##EL PROGRAMA ES EJECUTADO COMO CHILD DESDE EL TOPMENU
  CALL ui.Interface.setName("vehiculos")
  CALL ui.Interface.setType("child")
  CALL ui.Interface.setContainer("parent1")
  CALL ui.Interface.setText("Usuarios")
  CALL ui.Interface.refresh()
  DISPLAY ui.Interface.getChildInstances("vehiculos")
  LET nombre=ui.Interface.getName()
  DISPLAY nombre
END IF  


#VALIDANDO INFORMACION DE USUARIO Y PERFIL
CALL fgl_getenv("LOGNAME") RETURNING g_usuario

LET g_programa="vehiculos"
SELECT per_codperfil INTO g_perfil FROM top_usuarios 
WHERE usr_userid=g_usuario

SELECT * FROM top_perfiles WHERE per_codperfil=g_perfil 
IF STATUS=NOTFOUND THEN
  CALL fgl_winmessage("error","Perfil de Usuario No Existe!!","error")
  EXIT PROGRAM 
ELSE 
  SELECT FIRST 1 * FROM top_progxperfil WHERE per_codperfil=g_perfil
  AND opc_codigo IN (SELECT opc_codigo FROM top_programas WHERE opc_rout=g_programa)  
  IF STATUS=NOTFOUND THEN
    CALL fgl_winmessage("error","Perfil de Usuario No tiene acceso a este Programa!!","error")
    EXIT PROGRAM
  ELSE
    #TRAE PERMISOS DE OPCION DEL PROGRAMA ACTUAL DE ACUERDO AL PERFIL DE USUARIO
    DECLARE c_per CURSOR FOR
    SELECT b.oxp_name FROM top_progxperfil a,top_opcxprog b,top_programas c
    WHERE a.per_codperfil=g_perfil AND c.opc_rout=g_programa
    AND a.oxp_codigo=b.oxp_codigo and b.opc_codigo=c.opc_codigo
  END IF
END IF  

DISPLAY "sucursal "||arg_val(2)

##SI NO ESTA EN MODO CHILD PIDE POR UNA SUCURSAL DE TRABAJO
IF arg_val(2) IS NULL THEN
  CALL choose_sucursal2(g_usuario) RETURNING g_sucursal,g_nom_suc,g_company
ELSE 
  LET g_sucursal=arg_val(2)  
END IF


IF ui.Interface.getChildInstances("vehiculos")  > 0 THEN 
  CALL fgl_winmessage("Error","Ya existe una instancia de este programa!!","error")
  EXIT PROGRAM
ELSE 
  LET cuantos=ui.Interface.getChildInstances(ui.Interface.getName())
  DISPLAY "memo"||cuantos 
END IF  
  
  DEFER INTERRUPT 
  OPTIONS INPUT  WRAP 
  CLOSE WINDOW SCREEN
  LET g_existe_vehiculo=FALSE
  CALL ui.Interface.loadActionDefaults ("actions.4ad")
  CALL ui.Interface.loadToolBar ("toolbar.4tb")
  CALL ui.Interface.loadStyles("estilo_vales.4st")
  OPEN WINDOW f_vehiculo WITH FORM "f_vehiculo"
  LET windows = ui.Window.getCurrent()
  LET forma = windows.getForm()

    MENU
      BEFORE MENU
      CALL forma.setElementHidden("group2",1)
      CALL fgl_settitle("Vehiculos")
      WHENEVER ERROR CONTINUE
      CALL DIALOG.setActionActive("new",FALSE)
      CALL DIALOG.setActionActive("query",FALSE)
      FOREACH c_per INTO g_accion 
        CALL DIALOG.setActionActive(g_accion CLIPPED,TRUE)
      END FOREACH
      WHENEVER ERROR STOP  
      ON ACTION NEW
        CALL insertar_vehiculo()
      ON ACTION query
        CALL consultar_vehiculo()
      ON ACTION exit
    EXIT MENU
  END MENU
END MAIN

FUNCTION insertar_vehiculo()
DEFINE l_cadena STRING,
 l_pasa, l_tipo, l_count, l_codigo INTEGER,
 l_bandera BOOLEAN
CLEAR FORM   
DIALOG ATTRIBUTES(UNBUFFERED)
 INPUT BY NAME gr_vehiculo.veh_placa THRU gr_vehiculo.per_numidenti

   BEFORE INPUT
    CALL DIALOG.setFieldActive("veh_numinterno",FALSE )
    CALL DIALOG.setFieldActive("ciu_idciudad",FALSE)
    SELECT c.ciu_idciudad ,c.ciu_nomciudad, c.pai_idpais
    INTO gr_vehiculo.ciu_idciudad, gr_vehiculo.ciu_nomciudad, g_pais 
    FROM top_ciudad c, top_sucursal s 
    WHERE c.ciu_idciudad = s.ciu_idciudad 
    AND s.suc_cod = g_sucursal
    DISPLAY BY NAME gr_vehiculo.ciu_idciudad, gr_vehiculo.ciu_nomciudad
    SELECT est_id,est_descripcion INTO gr_vehiculo.est_id,gr_vehiculo.est_descripcion 
    FROM top_estado
    WHERE top_estado.est_id = 1 -- ACTIVO
    DISPLAY BY NAME gr_vehiculo.est_id, gr_vehiculo.est_descripcion
    CALL forma.setElementHidden("group2",1)
    CALL fgl_settitle("Adicionar Vehiculos")
        
    ON ACTION zoom
     CASE 
      WHEN INFIELD(tip_idtipo)
       LET l_codigo = gr_vehiculo.tip_idtipo  
       CALL make_help("top_tiposervicio","tip_idtipo", "tip_nomtipo", "TIPO SERVICIO", null) RETURNING gr_vehiculo.tip_idtipo
       CLOSE WINDOW w_help
       IF gr_vehiculo.tip_idtipo IS NOT NULL THEN 
        SELECT tip_nomtipo  INTO gr_vehiculo.tip_nomtipo FROM top_tiposervicio
        WHERE gr_vehiculo.tip_idtipo=top_tiposervicio.tip_idtipo
        DISPLAY BY NAME gr_vehiculo.tip_idtipo, gr_vehiculo.tip_nomtipo
       ELSE
        SELECT tip_idtipo,tip_nomtipo INTO gr_vehiculo.tip_idtipo,gr_vehiculo.tip_nomtipo 
        FROM top_tiposervicio
        WHERE top_tiposervicio.tip_idtipo = l_codigo
        DISPLAY BY NAME gr_vehiculo.tip_idtipo, gr_vehiculo.tip_nomtipo 
       END IF  
   
      WHEN INFIELD(lin_idlinea) 
       LET l_codigo = gr_vehiculo.lin_idlinea
       CALL make() RETURNING gr_vehiculo.lin_idlinea
       CLOSE WINDOW w_help
       IF gr_vehiculo.lin_idlinea > 0 THEN 
        SELECT top_marca.mar_nommarca, top_linea.lin_idlinea, top_linea.lin_nomlinea
        INTO gr_vehiculo.mar_nommarca, gr_vehiculo.lin_idlinea, gr_vehiculo.lin_nomlinea
        FROM top_marca, top_linea
        WHERE top_linea.lin_idlinea=gr_vehiculo.lin_idlinea
        AND  top_marca.mar_idmarca=top_linea.mar_idmarca
       ELSE 
        SELECT top_marca.mar_nommarca, top_linea.lin_idlinea, top_linea.lin_nomlinea
        INTO gr_vehiculo.mar_nommarca, gr_vehiculo.lin_idlinea, gr_vehiculo.lin_nomlinea
        FROM top_marca, top_linea
        WHERE top_linea.lin_idlinea = l_codigo
        AND top_marca.mar_idmarca = top_linea.mar_idmarca
        DISPLAY BY NAME gr_vehiculo.lin_idlinea, gr_vehiculo.lin_nomlinea
       END IF 
              
      WHEN INFIELD(cla_idclaseveh) 
       LET l_codigo = gr_vehiculo.lin_idlinea
       CALL make_help("top_clase_vehiculo","cla_idclaseveh", "cla_nomclase", "CLASE DE SERVICIO", null) RETURNING gr_vehiculo.cla_idclaseveh             
       CLOSE WINDOW w_help
       IF gr_vehiculo.cla_idclaseveh IS NOT NULL THEN 
        SELECT cla_nomclase INTO gr_vehiculo.cla_nomclase 
        FROM top_clase_vehiculo
        WHERE top_clase_vehiculo.cla_idclaseveh = gr_vehiculo.lin_idlinea 
        DISPLAY BY NAME gr_vehiculo.cla_idclaseveh, gr_vehiculo.cla_nomclase
       ELSE 
        SELECT cla_idclaseveh,cla_nomclase INTO gr_vehiculo.lin_idlinea,gr_vehiculo.cla_nomclase 
        FROM top_clase_vehiculo
        WHERE top_clase_vehiculo.cla_idclaseveh = l_codigo
        DISPLAY BY NAME gr_vehiculo.cla_idclaseveh, gr_vehiculo.cla_nomclase
       END IF  
              
      WHEN INFIELD(car_idcarroce)
       LET l_codigo = gr_vehiculo.car_idcarroce 
       CALL make_help("top_carroceria","car_idcarroce", "car_nomcarroce", "CARROCERIAS", null) RETURNING gr_vehiculo.car_idcarroce
       CLOSE WINDOW w_help
       IF gr_vehiculo.car_idcarroce IS NOT NULL THEN 
        SELECT top_carroceria.car_nomcarroce  INTO gr_vehiculo.car_nomcarroce FROM top_carroceria
        WHERE gr_vehiculo.car_idcarroce=top_carroceria.car_idcarroce
        DISPLAY BY NAME gr_vehiculo.car_idcarroce, gr_vehiculo.car_nomcarroce
       ELSE 
        SELECT car_idcarroce,car_nomcarroce  INTO gr_vehiculo.car_idcarroce,gr_vehiculo.car_nomcarroce 
        FROM top_carroceria
        WHERE top_carroceria.car_idcarroce = l_codigo
        DISPLAY BY NAME gr_vehiculo.car_idcarroce, gr_vehiculo.car_nomcarroce
       END IF 

      WHEN INFIELD(col_idcolor)
       LET l_codigo = gr_vehiculo.col_idcolor 
       CALL make_help("top_color","col_idcolor", "col_nomcolor", "COLORES", null) RETURNING gr_vehiculo.col_idcolor
       CLOSE WINDOW w_help
       IF gr_vehiculo.col_idcolor IS NOT NULL THEN 
        SELECT top_color.col_nomcolor  INTO gr_vehiculo.col_nomcolor FROM top_color
        WHERE gr_vehiculo.col_idcolor=top_color.col_idcolor
        DISPLAY BY NAME gr_vehiculo.col_idcolor, gr_vehiculo.col_nomcolor
       ELSE
        SELECT col_idcolor,col_nomcolor INTO gr_vehiculo.col_idcolor, gr_vehiculo.col_nomcolor 
        FROM top_color
        WHERE top_color.col_idcolor = l_codigo 
        DISPLAY BY NAME gr_vehiculo.col_idcolor, gr_vehiculo.col_nomcolor
       END IF 
               
      WHEN INFIELD(cap_idcapaci)
       LET l_codigo = gr_vehiculo.cap_idcapaci 
       CALL make_help("top_capacidad","cap_idcapaci", "cap_nomcapaci", "CAPACIDAD", null) RETURNING gr_vehiculo.cap_idcapaci
       CLOSE WINDOW w_help
       IF gr_vehiculo.cap_idcapaci IS NOT NULL THEN  
        SELECT top_capacidad.cap_nomcapaci  INTO gr_vehiculo.cap_nomcapaci FROM top_capacidad
        WHERE gr_vehiculo.cap_idcapaci=top_capacidad.cap_idcapaci
        DISPLAY BY NAME gr_vehiculo.cap_idcapaci, gr_vehiculo.cap_nomcapaci
       ELSE 
        SELECT cap_idcapaci,cap_nomcapaci INTO gr_vehiculo.cap_idcapaci,gr_vehiculo.cap_nomcapaci 
        FROM top_capacidad
        WHERE top_capacidad.cap_idcapaci = l_codigo
        DISPLAY BY NAME gr_vehiculo.cap_idcapaci, gr_vehiculo.cap_nomcapaci
       END IF 
              
      WHEN INFIELD(com_idcombus) 
       LET l_codigo = gr_vehiculo.com_idcombus
       CALL make_help("top_combustible","com_idcombus", "com_nomcombus", "COMBUSTIBLE", null) RETURNING gr_vehiculo.com_idcombus
       CLOSE WINDOW w_help
       IF gr_vehiculo.com_idcombus IS NOT NULL THEN 
        SELECT com_nomcombus  INTO gr_vehiculo.com_nomcombus FROM top_combustible
        WHERE gr_vehiculo.com_idcombus=top_combustible.com_idcombus
        DISPLAY BY NAME gr_vehiculo.com_idcombus, gr_vehiculo.com_nomcombus
       ELSE 
        SELECT com_idcombus,com_nomcombus  INTO gr_vehiculo.com_idcombus,gr_vehiculo.com_nomcombus 
        FROM top_combustible
        WHERE top_combustible.com_idcombus = l_codigo
        DISPLAY BY NAME gr_vehiculo.com_idcombus, gr_vehiculo.com_nomcombus
       END IF  
             
      WHEN INFIELD(emp_idempresa) 
       CALL make3() RETURNING gr_vehiculo.emp_idempresa
       CLOSE WINDOW w_help4
       SELECT emp_nomempresa INTO gr_vehiculo.emp_nomempresa
       FROM top_empresa 
       WHERE suc_cod = g_sucursal AND tip_idtipoem = 1 
       AND emp_idempresa =  gr_vehiculo.emp_idempresa
       DISPLAY BY NAME gr_vehiculo.emp_idempresa, gr_vehiculo.emp_nomempresa
           
      WHEN INFIELD(sit_codsitio) 
       LET l_codigo = gr_vehiculo.sit_codsitio
       CALL make_help("top_sitio","sit_codsitio", "sit_nomsitio", "SITIOS", null) RETURNING gr_vehiculo.sit_codsitio
       CLOSE WINDOW w_help
       IF gr_vehiculo.sit_codsitio IS NOT NULL THEN 
        SELECT top_sitio.sit_nomsitio  INTO gr_vehiculo.sit_nomsitio FROM top_sitio
        WHERE top_sitio.sit_codsitio=gr_vehiculo.sit_codsitio
        DISPLAY BY NAME gr_vehiculo.sit_codsitio, gr_vehiculo.sit_nomsitio
       ELSE 
        SELECT sit_codsitio,sit_nomsitio  INTO gr_vehiculo.sit_codsitio,gr_vehiculo.sit_nomsitio 
        FROM top_sitio
        WHERE top_sitio.sit_codsitio = l_codigo
        DISPLAY BY NAME gr_vehiculo.sit_codsitio, gr_vehiculo.sit_nomsitio
       END IF 

      WHEN INFIELD(per_numidenti)
       IF gr_vehiculo.per_numidenti IS NOT NULL THEN
        PREPARE s2 FROM "EXECUTE FUNCTION quitar_caracteres_especiales(?)"
        EXECUTE s2 INTO gr_vehiculo.per_numidenti USING gr_vehiculo.per_numidenti
        SELECT top_persona.per_numidenti , top_persona.per_nombre 
        INTO gr_vehiculo.per_numidenti,  gr_vehiculo.per_nombre
        FROM top_persona
        WHERE top_persona.per_numidenti = gr_vehiculo.per_numidenti
        AND top_persona.suc_cod = g_sucursal
        IF STATUS = NOTFOUND THEN
         CALL forma.setElementHidden("label22",1)
         CALL forma.setFieldHidden("top_persona.per_nombre",1)
         LET l_cadena = "fglrun persona " ||" "|| gr_vehiculo.per_tipoidenti||" "|| gr_vehiculo.per_numidenti||" "|| g_sucursal
         DISPLAY l_cadena 
         RUN l_cadena RETURNING l_pasa
         IF l_pasa=0 THEN 
          CALL fgl_winmessage("Info","Inserto Persona", "information")
          DISPLAY BY NAME gr_vehiculo.per_numidenti,  gr_vehiculo.per_nombre
         ELSE
          CALL fgl_winmessage("Info","NO Inserto Persona", "information")
          NEXT FIELD per_numidenti 
         END IF
        ELSE
         LET l_pasa=0
         DISPLAY BY NAME gr_vehiculo.per_numidenti, gr_vehiculo.per_nombre
        END IF
       ELSE 
        CALL fgl_winmessage("Info","La cedula no puede ir vacia", "information")
       END IF   
              
     END CASE   
             
     AFTER FIELD veh_placa
      IF gr_vehiculo.veh_placa IS NULL THEN
       CALL fgl_winmessage("Info","Por Favor Digite Placa", "information")
       NEXT FIELD veh_placa
      END IF
      IF gr_vehiculo.veh_placa IS NOT NULL THEN
       IF g_pais = 57 THEN --Esto aplica solo para Colombia
        IF length(gr_vehiculo.veh_placa) = 6 THEN
         CALL valida_placa(gr_vehiculo.veh_placa) RETURNING l_bandera 
         IF NOT l_bandera THEN
          CALL fgl_winmessage("Info","LA PLACA DEBE CONTENER 3 LETRAS Y 3 NUMEROS !!!", "information")
          NEXT FIELD veh_placa 
         END IF
        ELSE 
         CALL fgl_winmessage("Info","LA PLACA DEBE CONTENER 3 LETRAS Y 3 NUMEROS !!!", "information")
         NEXT FIELD veh_placa
        END IF  
       END IF     
       CALL existe_dato ("top_vehiculo","veh_placa", gr_vehiculo.veh_placa) RETURNING g_existe_dato
       IF g_existe_dato THEN
        CALL fgl_winmessage("Info","Placa ya Existe", "information")
        NEXT FIELD veh_placa
       ELSE
        DISPLAY gr_vehiculo.veh_placa
        NEXT FIELD tip_idtipo
       END IF
      END IF 
        
      AFTER FIELD tip_idtipo
       IF gr_vehiculo.tip_idtipo IS NOT NULL THEN
        SELECT tip_nomtipo  INTO gr_vehiculo.tip_nomtipo FROM top_tiposervicio
        WHERE gr_vehiculo.tip_idtipo=top_tiposervicio.tip_idtipo
        IF sqlca.sqlcode <> 0 THEN 
         CALL fgl_winmessage("Info","Ese codigo de Tipo de Servicio NO existe", "information")
         LET gr_vehiculo.tip_nomtipo = NULL
         NEXT FIELD tip_idtipo
        ELSE
         DISPLAY BY NAME gr_vehiculo.tip_idtipo, gr_vehiculo.tip_nomtipo
        END IF
       END IF  
        
      AFTER FIELD lin_idlinea
       IF gr_vehiculo.lin_idlinea IS NOT NULL THEN  
        SELECT  top_marca.mar_nommarca, top_linea.lin_idlinea, top_linea.lin_nomlinea
        INTO gr_vehiculo.mar_nommarca, gr_vehiculo.lin_idlinea,  gr_vehiculo.lin_nomlinea
        FROM top_marca, top_linea
        WHERE top_linea.lin_idlinea=gr_vehiculo.lin_idlinea
        AND top_marca.mar_idmarca=top_linea.mar_idmarca 
        IF sqlca.sqlcode <> 0 THEN 
         CALL fgl_winmessage("Info","Ese codigo de Marca  NO existe", "information")
         LET gr_vehiculo.mar_nommarca = NULL
         NEXT FIELD lin_idlinea
        ELSE
         DISPLAY BY NAME gr_vehiculo.lin_idlinea, gr_vehiculo.mar_nommarca, gr_vehiculo.lin_nomlinea
         LET imagelinea = gr_vehiculo.lin_idlinea
         DISPLAY BY NAME imagelinea
        END IF
       END IF  
        
      AFTER FIELD col_idcolor
       IF  gr_vehiculo.col_idcolor IS NOT NULL THEN
        SELECT top_color.col_nomcolor  INTO gr_vehiculo.col_nomcolor FROM top_color
        WHERE gr_vehiculo.col_idcolor=top_color.col_idcolor
        IF sqlca.sqlcode <> 0 THEN 
         CALL fgl_winmessage("Info","Ese codigo de Color NO existe", "information")
         LET gr_vehiculo.col_nomcolor = NULL
         NEXT FIELD col_idcolor
        ELSE
         DISPLAY BY NAME gr_vehiculo.col_idcolor, gr_vehiculo.col_nomcolor
        END IF
       END IF
        
      AFTER FIELD cla_idclaseveh
       IF gr_vehiculo.cla_idclaseveh IS NOT NULL THEN 
        SELECT top_clase_vehiculo.cla_nomclase  INTO gr_vehiculo.cla_nomclase FROM top_clase_vehiculo
        WHERE gr_vehiculo.cla_idclaseveh=top_clase_vehiculo.cla_idclaseveh
        IF sqlca.sqlcode <> 0 THEN 
         CALL fgl_winmessage("Info","Ese codigo de Clase de Vehiculo NO existe", "information")
         LET gr_vehiculo.cla_nomclase = NULL
         NEXT FIELD cla_idclaseveh
        ELSE
         DISPLAY BY NAME gr_vehiculo.car_idcarroce, gr_vehiculo.car_nomcarroce
        END IF
       END IF 
       
      AFTER FIELD car_idcarroce
       IF gr_vehiculo.car_idcarroce IS NOT NULL THEN
        SELECT top_carroceria.car_nomcarroce  INTO gr_vehiculo.car_nomcarroce FROM top_carroceria
        WHERE gr_vehiculo.car_idcarroce=top_carroceria.car_idcarroce
        IF sqlca.sqlcode <> 0 THEN 
         CALL fgl_winmessage("Info","Ese codigo de Carroceria NO existe", "information")
         LET gr_vehiculo.car_nomcarroce = NULL
         NEXT FIELD car_idcarroce
        ELSE
         DISPLAY BY NAME gr_vehiculo.car_idcarroce, gr_vehiculo.car_nomcarroce
        END IF
       END IF 
        
      AFTER FIELD cap_idcapaci
       IF gr_vehiculo.cap_idcapaci IS NOT NULL THEN
        SELECT top_capacidad.cap_nomcapaci  INTO gr_vehiculo.cap_nomcapaci FROM top_capacidad
        WHERE gr_vehiculo.cap_idcapaci=top_capacidad.cap_idcapaci
        IF sqlca.sqlcode <> 0 THEN 
         CALL fgl_winmessage("Info","Ese codigo de Capacidad NO existe", "information")
         LET gr_vehiculo.cap_nomcapaci = NULL
         NEXT FIELD cap_idcapaci
        ELSE
         DISPLAY BY NAME gr_vehiculo.cap_idcapaci, gr_vehiculo.cap_nomcapaci
        END IF 
       END IF   
        
      AFTER FIELD com_idcombus
       IF gr_vehiculo.com_idcombus IS NOT NULL THEN 
        SELECT top_combustible.com_nomcombus  INTO gr_vehiculo.com_nomcombus FROM top_combustible
        WHERE gr_vehiculo.com_idcombus=top_combustible.com_idcombus
        IF sqlca.sqlcode <> 0 THEN 
         CALL fgl_winmessage("Info","Ese codigo de Combustible no existe", "information")
         LET gr_vehiculo.com_nomcombus = NULL
         NEXT FIELD com_idcombus
        ELSE
         DISPLAY BY NAME gr_vehiculo.com_idcombus, gr_vehiculo.com_nomcombus
         NEXT FIELD veh_motor
        END IF
       END IF       
        
      AFTER FIELD veh_motor
       IF gr_vehiculo.veh_motor IS NOT NULL THEN
        CALL existe_dato ("top_vehiculo","veh_motor", gr_vehiculo.veh_motor) RETURNING g_existe_dato
        IF g_existe_dato THEN
         CALL fgl_winmessage("Info","Motor ya Existe", "information")
         NEXT FIELD  veh_motor
        ELSE
         NEXT FIELD veh_serie
        END IF 
       END IF 
        
      AFTER FIELD veh_serie
       IF gr_vehiculo.veh_serie IS NOT NULL THEN
        CALL existe_dato ("top_vehiculo","veh_serie", gr_vehiculo.veh_serie) RETURNING g_existe_dato
        IF g_existe_dato THEN
         CALL fgl_winmessage("Info","Numero de serie  ya existe", "information")
         NEXT FIELD veh_serie
        ELSE
         NEXT FIELD veh_modelo
        END IF
       END IF
        
      AFTER FIELD veh_modelo
       IF gr_vehiculo.veh_modelo IS NOT NULL THEN
        IF gr_vehiculo.veh_modelo<1990 OR gr_vehiculo.veh_modelo>YEAR(CURRENT)+ 1 THEN
         CALL fgl_winmessage("Info","Modelo NO Válido", "information")
         NEXT FIELD veh_modelo
        END IF
       END IF
        
      AFTER FIELD veh_cilindraje
       IF gr_vehiculo.veh_cilindraje IS NOT NULL THEN
        IF gr_vehiculo.veh_cilindraje <80 OR gr_vehiculo.veh_cilindraje>7000 THEN
         CALL fgl_winmessage("Info","Cilindraje NO Válido", "information")
         NEXT FIELD veh_cilindraje
        END IF
       END IF
         
      AFTER FIELD veh_orden
       IF gr_vehiculo.veh_orden IS NOT NULL THEN
        CALL existe_orden (g_sucursal, gr_vehiculo.veh_orden) RETURNING g_existe_orden
        IF g_existe_orden THEN
         CALL fgl_winmessage("Info","Numero de orden ya asignado", "information")
         NEXT FIELD veh_orden
        ELSE
         NEXT FIELD emp_idempresa
        END IF
       END IF
          
      AFTER FIELD emp_idempresa
       IF gr_vehiculo.emp_idempresa IS NOT NULL THEN
        SELECT top_empresa.emp_nomempresa  INTO gr_vehiculo.emp_nomempresa FROM top_empresa
        WHERE suc_cod = g_sucursal AND tip_idtipoem = 1
        AND emp_idempresa = gr_vehiculo.emp_idempresa
        IF sqlca.sqlcode <> 0 THEN 
         CALL fgl_winmessage("Info","Ese codigo de Empresa NO existe", "information")
         LET gr_vehiculo.emp_nomempresa = NULL
         NEXT FIELD emp_idempresa
        ELSE
         DISPLAY BY NAME gr_vehiculo.emp_idempresa, gr_vehiculo.emp_nomempresa
         NEXT FIELD sit_codsitio
        END IF
       END IF
            
      AFTER FIELD sit_codsitio
       IF gr_vehiculo.sit_codsitio IS NOT NULL THEN
        SELECT top_sitio.sit_nomsitio INTO gr_vehiculo.sit_nomsitio 
        FROM top_sitio
        WHERE top_sitio.sit_codsitio=gr_vehiculo.sit_codsitio
        IF STATUS = NOTFOUND THEN 
         CALL fgl_winmessage("Info","Ese codigo de Sede NO existe", "information")
         LET gr_vehiculo.sit_nomsitio = NULL
         NEXT FIELD sit_codsitio
        ELSE
         IF gr_vehiculo.sit_codsitio <> 1 THEN
          SELECT top_estado.est_descripcion  
          INTO gr_vehiculo.est_descripcion 
          FROM top_estado
          WHERE top_estado.est_id=1
          LET gr_vehiculo.est_id=1
          CALL DIALOG.setFieldActive("veh_numinterno",TRUE)
          NEXT FIELD top_vehiculo.veh_numinterno
         ELSE
          DISPLAY BY NAME gr_vehiculo.sit_codsitio, gr_vehiculo.sit_nomsitio
          LET gr_vehiculo.veh_numinterno=0
          LET gr_vehiculo.veh_fechafilia=CURRENT 
          LET gr_vehiculo.veh_fechadesa=NULL
          SELECT top_estado.est_descripcion  INTO gr_vehiculo.est_descripcion FROM top_estado
          WHERE top_estado.est_id=1
          LET gr_vehiculo.est_id=1
          NEXT FIELD per_tipoidenti
         END IF
        END IF
       END IF

      AFTER FIELD veh_numinterno
       IF gr_vehiculo.veh_numinterno IS NOT NULL THEN 
        IF gr_vehiculo.sit_codsitio <> 1 THEN 
         SELECT COUNT(*)
         INTO l_count  
         FROM top_vehiculo
         WHERE sit_codsitio = gr_vehiculo.sit_codsitio 
         AND veh_numinterno = gr_vehiculo.veh_numinterno
         AND suc_cod = g_sucursal
         IF l_count >= 1 THEN
          CALL DIALOG.setFieldActive("veh_numinterno",TRUE )
          CALL fgl_winmessage("Info","Numero Interno ya existe", "information")
          NEXT FIELD veh_numinterno
         ELSE 
          NEXT FIELD per_tipoidenti
         END IF
        END IF 
       END IF 
       
      AFTER FIELD per_tipoidenti 
       IF gr_vehiculo.per_tipoidenti  IS NOT NULL THEN
        NEXT FIELD per_numidenti 
       END IF
        
   END INPUT

   DISPLAY ARRAY arr_persona TO sr_persona.* ATTRIBUTES(COUNT=arr_persona.getLength())
   END  DISPLAY
       
   ON ACTION save
    LET l_pasa=1
    IF gr_vehiculo.veh_placa IS NULL THEN
     CALL fgl_winmessage("Info","Complete Placa  ", "information")
     NEXT FIELD veh_placa
    ELSE 
     IF g_pais = 57 THEN --Esto aplica solo para Colombia
      IF length(gr_vehiculo.veh_placa) = 6 THEN
       CALL valida_placa(gr_vehiculo.veh_placa) RETURNING l_bandera 
       IF NOT l_bandera THEN
        CALL fgl_winmessage("Info","LA PLACA DEBE CONTENER 3 LETRAS Y 3 NUMEROS !!!", "information")
        NEXT FIELD veh_placa
       END IF  
      ELSE 
       CALL fgl_winmessage("Info","LA PLACA DEBE CONTENER 3 LETRAS Y 3 NUMEROS !!!", "information")
       NEXT FIELD veh_placa
      END IF
     ELSE 
      CALL existe_dato ("top_vehiculo","veh_placa", gr_vehiculo.veh_placa) RETURNING g_existe_dato
      IF g_existe_dato THEN
       CALL fgl_winmessage("Info","Placa ya Existe", "information")
       NEXT FIELD veh_placa
      ELSE
       DISPLAY gr_vehiculo.veh_placa       
      END IF 
     END IF  
    END IF 
    
    IF gr_vehiculo.ciu_idciudad IS NULL THEN
     CALL fgl_winmessage("Info","Complete Ciudad  ", "information")
     NEXT FIELD ciu_idciudad
    END IF
       
    IF gr_vehiculo.tip_idtipo IS NULL THEN
     CALL fgl_winmessage("Info","Complete Tipo de Servicio  ", "information")
     NEXT FIELD tip_idtipo
    END IF

    IF gr_vehiculo.lin_idlinea IS NULL THEN
     CALL fgl_winmessage("Info","Complete Marca y Linea  ", "information")
     NEXT FIELD lin_idlinea
    END IF

    IF gr_vehiculo.col_idcolor IS NULL THEN
     CALL fgl_winmessage("Info","Complete Color  ", "information")
     NEXT FIELD col_idcolor
    END IF

    IF gr_vehiculo.car_idcarroce IS NULL THEN
     CALL fgl_winmessage("Info","Complete Carroceria  ", "information")
     NEXT FIELD car_idcarroce
    END IF
       
    IF gr_vehiculo.cla_idclaseveh IS NULL THEN
     CALL fgl_winmessage("Info","Complete Clase Vehículo  ", "information")
     NEXT FIELD car_idcarroce
    END IF
       
    IF gr_vehiculo.cap_idcapaci IS NULL THEN
     CALL fgl_winmessage("Info","Complete Capacidad  ", "information")
     NEXT FIELD cap_idcapaci
    END IF
       
    IF gr_vehiculo.com_idcombus IS NULL THEN
     CALL fgl_winmessage("Info","Complete Tipo Combustible  ", "information")
     NEXT FIELD com_idcombus
    END IF

    IF gr_vehiculo.veh_motor IS NULL THEN
     CALL fgl_winmessage("Info","Complete N. Motor  ", "information")
     NEXT FIELD veh_motor
    ELSE
     CALL existe_dato ("top_vehiculo","veh_motor", gr_vehiculo.veh_motor) RETURNING g_existe_dato
     IF g_existe_dato THEN
      CALL fgl_winmessage("Info","Motor ya Existe", "information")
      NEXT FIELD veh_motor
     END IF  
    END IF
       
    IF gr_vehiculo.veh_serie IS NULL THEN
     CALL fgl_winmessage("Info","Complete  Serie  ", "information")
     NEXT FIELD veh_serie
    ELSE
     CALL existe_dato ("top_vehiculo","veh_serie", gr_vehiculo.veh_serie) RETURNING g_existe_dato
     IF g_existe_dato THEN
      CALL fgl_winmessage("Info","Numero de serie  ya existe", "information")
      NEXT FIELD veh_serie
     END IF
    END IF

    IF gr_vehiculo.veh_modelo IS NULL THEN
     CALL fgl_winmessage("Info","Complete  Modelo  ", "information")
     NEXT FIELD veh_modelo
    END IF

    IF gr_vehiculo.veh_cilindraje IS NULL THEN
     CALL fgl_winmessage("Info","Complete Cilindraje  ", "information")
     NEXT FIELD veh_cilindraje
    END IF

    IF gr_vehiculo.veh_orden IS NULL THEN
     CALL fgl_winmessage("Info","Complete N. Orden  ", "information")
     NEXT FIELD veh_orden
    ELSE
     CALL existe_orden (g_sucursal, gr_vehiculo.veh_orden) RETURNING g_existe_orden
     IF g_existe_orden THEN
      CALL fgl_winmessage("Info","Numero de orden ya asignado", "information")
      NEXT FIELD veh_orden
     END IF
    END IF

    IF gr_vehiculo.sit_codsitio <> 1 THEN
     IF gr_vehiculo.veh_numinterno <> 0 THEN      
      SELECT COUNT(*)
      INTO l_count  
      FROM top_vehiculo
      WHERE sit_codsitio = gr_vehiculo.sit_codsitio 
      AND veh_numinterno = gr_vehiculo.veh_numinterno
      AND suc_cod = g_sucursal
      IF l_count >= 1 THEN
       CALL DIALOG.setFieldActive("veh_numinterno",TRUE )
       CALL fgl_winmessage("Info","Numero Interno ya existe", "information")
       NEXT FIELD veh_numinterno
      ELSE
       DISPLAY gr_vehiculo.veh_numinterno
       LET gr_vehiculo.veh_fechafilia=CURRENT 
       LET gr_vehiculo.veh_fechadesa=NULL
       DISPLAY BY NAME gr_vehiculo.veh_fechafilia, gr_vehiculo.veh_fechadesa
       SELECT top_estado.est_descripcion  INTO gr_vehiculo.est_descripcion FROM top_estado
       WHERE top_estado.est_id=1
       LET gr_vehiculo.est_id=1
       DISPLAY BY NAME gr_vehiculo.est_id, gr_vehiculo.est_descripcion
      END IF
     ELSE 
      CALL fgl_winmessage("Info","Numero Interno no puede ser cero", "information")
      NEXT FIELD veh_numinterno
     END IF  
    END IF
       
    IF gr_vehiculo.veh_fechafilia IS NULL THEN
     CALL fgl_winmessage("Info","Complete Fecha Afiliación  ", "information")
     NEXT FIELD veh_fechafilia
    END IF

    IF gr_vehiculo.emp_idempresa IS NULL THEN
     CALL fgl_winmessage("Info","Complete Empre. Afiliadora  ", "information")
     NEXT FIELD emp_idempresa
    END IF   
       
    IF gr_vehiculo.per_numidenti IS NULL THEN
     CALL fgl_winmessage("Info","Digite cédula propietario  ", "information")
     NEXT FIELD per_numidenti
    ELSE
     PREPARE s3 FROM "EXECUTE FUNCTION quitar_caracteres_especiales(?)"
     EXECUTE s3 INTO gr_vehiculo.per_numidenti USING gr_vehiculo.per_numidenti
     SELECT top_persona.per_numidenti , top_persona.per_nombre 
     INTO gr_vehiculo.per_numidenti,  gr_vehiculo.per_nombre
     FROM top_persona
     WHERE top_persona.per_numidenti = gr_vehiculo.per_numidenti
     AND top_persona.suc_cod = g_sucursal                         
     IF STATUS = NOTFOUND THEN
      CALL forma.setElementHidden("label22",1)
      CALL forma.setFieldHidden("top_persona.per_nombre",1)
      LET l_cadena = "fglrun persona " ||" "|| gr_vehiculo.per_tipoidenti||" "|| gr_vehiculo.per_numidenti||" "|| g_sucursal
      DISPLAY l_cadena 
      RUN l_cadena RETURNING l_pasa
      IF l_pasa=0 THEN 
       CALL fgl_winmessage("Info","Inserto Persona", "information")
       DISPLAY BY NAME gr_vehiculo.per_numidenti,  gr_vehiculo.per_nombre
      ELSE
       CALL fgl_winmessage("Info","NO Inserto Persona", "information")
       NEXT FIELD per_numidenti 
      END IF
     ELSE
      LET l_pasa=0
      DISPLAY BY NAME gr_vehiculo.per_numidenti, gr_vehiculo.per_nombre
     END IF
    END IF

    IF l_pasa <> 0 AND gr_vehiculo.per_nombre IS NULL THEN
     CALL fgl_winmessage("Info","Propietario no puede ir Vacio", "information")
     NEXT FIELD per_numidenti
    END IF

    IF gr_vehiculo.per_nombre IS NOT NULL THEN
     LET l_pasa = 0 
    END IF 
       
    SELECT username INTO g_usuario FROM sysusers WHERE username = USER
    IF g_pais = 52 THEN
     LET l_tipo= 6
    ELSE
     LET l_tipo= 1
    END IF

    BEGIN WORK
    LET l_cadena = gr_vehiculo.veh_placa
    LET gr_vehiculo.veh_placa = l_cadena.trim()
        
    WHENEVER ERROR CONTINUE 
    INSERT INTO top_vehiculo (veh_placa, suc_cod, tip_idtipo, lin_idlinea, col_idcolor, 
                                 cla_idclaseveh, car_idcarroce, cap_idcapaci, com_idcombus, veh_motor, 
                                 veh_serie, veh_modelo,   veh_cilindraje,   veh_orden, veh_fechafilia,
                                 est_id, veh_fechadesa, emp_idempresa, sit_codsitio, veh_numinterno, veh_fechactual, veh_usuario) 
                                
    VALUES (gr_vehiculo.veh_placa, g_sucursal, gr_vehiculo.tip_idtipo, gr_vehiculo.lin_idlinea,
                gr_vehiculo.col_idcolor, gr_vehiculo.cla_idclaseveh, gr_vehiculo.car_idcarroce, gr_vehiculo.cap_idcapaci, 
                gr_vehiculo.com_idcombus, gr_vehiculo.veh_motor, gr_vehiculo.veh_serie, gr_vehiculo.veh_modelo, 
                gr_vehiculo.veh_cilindraje, gr_vehiculo.veh_orden, gr_vehiculo.veh_fechafilia,gr_vehiculo.est_id, 
                gr_vehiculo.veh_fechadesa, gr_vehiculo.emp_idempresa, gr_vehiculo.sit_codsitio, gr_vehiculo.veh_numinterno,
                NULL, g_usuario)
    LET gr_vehiculo.veh_codigo = sqlca.sqlerrd[2]   
    IF SQLCA.SQLCODE<>0 THEN
     CALL FGL_WINMESSAGE( "STOP", "NO SE PUEDE INSERTAR VEHICULO.....COMUNIQUESE CON TECNOLOGIA", "error")
     ERROR SQLCA.SQLCODE||' '||SQLERRMESSAGE
     ROLLBACK WORK
    ELSE
     IF l_pasa = 0 THEN 
      INSERT INTO top_persovehiculo (per_numidenti, tip_idtipoper,veh_codigo, per_fechavincula, per_usuario, per_pagador)
      VALUES (gr_vehiculo.per_numidenti, l_tipo, gr_vehiculo.veh_codigo, CURRENT YEAR TO SECOND, g_usuario,1 )
      IF SQLCA.SQLCODE<>0 THEN
       CALL FGL_WINMESSAGE( "STOP", "NO SE PUEDE INSERTAR EL PROPIETARIO .....COMUNIQUESE CON TECNOLOGIA", "error")
       ERROR SQLCA.SQLCODE||' '||SQLERRMESSAGE
       ROLLBACK WORK
      ELSE 
       CALL FGL_WINMESSAGE( "INFO", "SE INSERTO EL PROPIETARIO", "info")
       CALL FGL_WINMESSAGE( "INFO", "VEHICULO ALMACENADO", "info")
       CALL forma.setElementHidden("group2",0)
       CALL array_persona()
       COMMIT WORK
       EXIT DIALOG 
      END IF   
     END IF 
    END IF

    ON ACTION CANCEL
     CALL FGL_WINMESSAGE( "INFO", "Usuario Cancelo Insertar", "info")
     CALL fgl_settitle("Vehiculos")
     INITIALIZE gr_vehiculo.* TO NULL
     LET imagelinea = NULL
     CLEAR FORM
     EXIT DIALOG

END DIALOG

END FUNCTION


FUNCTION existe_orden(l_suc_cod, l_veh_orden)
  DEFINE 
    l_veh_orden LIKE top_vehiculo.veh_orden,
    l_suc_cod LIKE top_vehiculo.suc_cod,
    l_contador INTEGER
  
    SELECT COUNT(*) INTO l_contador FROM top_vehiculo 
    WHERE top_vehiculo.suc_cod = l_suc_cod
    AND top_vehiculo.veh_orden = l_veh_orden 
    AND top_vehiculo.veh_placa <> gr_vehiculo.veh_placa
    
    IF l_contador > 0 THEN  
        LET g_existe_orden = TRUE 
        DISPLAY g_existe_orden, "TRUE"
      ELSE
        LET g_existe_orden = FALSE 
        DISPLAY g_existe_orden, "FALSE"
    END IF
    RETURN  g_existe_orden
END FUNCTION

FUNCTION existe_dato(l_tabla, l_campo, l_var)
  DEFINE 
    l_query, l_tabla CHAR (100),
    l_campo, l_var CHAR(25),
    l_contador INTEGER 
    
    LET l_contador = 0
    LET l_query = "SELECT COUNT(*) FROM ", l_tabla CLIPPED,
    " WHERE ", l_campo CLIPPED, " =?"
    PREPARE ext_sql FROM l_query
    
    EXECUTE ext_sql USING l_var INTO l_contador
    IF l_contador > 0 THEN  
        LET g_existe_dato = TRUE 
      ELSE
        LET g_existe_dato = FALSE 
    END IF
    
RETURN g_existe_dato
END FUNCTION


FUNCTION consultar_vehiculo()
  DEFINE ext_query, mysql CHAR(1400),
    l_actual, l_total SMALLINT,
    l_bandera BOOLEAN
    CALL fgl_settitle ("Consultar  Vehiculos")
    CALL forma.setElementHidden("group2",0)
    INITIALIZE gr_vehiculo.* TO NULL
    INITIALIZE arr_persona TO NULL
    CLEAR FORM 
    DIALOG ATTRIBUTES (UNBUFFERED)
    
    CONSTRUCT BY NAME ext_query ON  v.veh_placa, v.veh_numinterno, v.emp_idempresa, v.est_id, v.veh_fechafilia
    BEFORE CONSTRUCT 
      CALL DIALOG.setActionActive("fistrow",FAlSE)
      CALL DIALOG.setActionActive("nextrow",FALSE)
      CALL DIALOG.setActionActive("lastrow",FALSE)
      CALL DIALOG.setActionActive("prevrow",FALSE)
      CALL DIALOG.setActionActive("modify",FALSE)
      CALL DIALOG.setActionActive("eraser",FALSE)
      CALL DIALOG.setActionActive("printer",FALSE)
      SELECT c.ciu_idciudad ,c.ciu_nomciudad,c.pai_idpais
      INTO gr_vehiculo.ciu_idciudad, gr_vehiculo.ciu_nomciudad, g_pais 
      FROM top_ciudad c, top_sucursal s 
      WHERE c.ciu_idciudad = s.ciu_idciudad 
      AND s.suc_cod = g_sucursal
      DISPLAY BY NAME gr_vehiculo.ciu_idciudad, gr_vehiculo.ciu_nomciudad

      ON ACTION zoom
      IF INFIELD(emp_idempresa) THEN
        CALL make3() RETURNING gr_vehiculo.emp_idempresa
        CLOSE WINDOW w_help4
        IF gr_vehiculo.emp_idempresa > 0 THEN 
          SELECT emp_nomempresa INTO gr_vehiculo.emp_nomempresa
          FROM top_empresa 
          WHERE suc_cod = g_sucursal AND tip_idtipoem = 1 
          AND emp_idempresa =  gr_vehiculo.emp_idempresa
          DISPLAY BY NAME gr_vehiculo.emp_idempresa, gr_vehiculo.emp_nomempresa
        END IF 
      END IF
      IF INFIELD(est_id) THEN
        CALL make_help("top_estado","est_id", "est_descripcion", "ESTADO", NULL) RETURNING gr_vehiculo.est_id
        CLOSE WINDOW w_help
        IF gr_vehiculo.est_id IS NOT NULL THEN
          SELECT  est_descripcion INTO gr_vehiculo.est_descripcion 
          FROM top_estado
          WHERE est_id = gr_vehiculo.est_id
          DISPLAY BY NAME gr_vehiculo.est_id, gr_vehiculo.est_descripcion 
        END IF 
      END IF 

    ON ACTION ACCEPT 
      LET mysql = "SELECT v.veh_placa, c.ciu_idciudad, c.ciu_nomciudad, v.tip_idtipo, ",  
      "s.tip_nomtipo,v.lin_idlinea, m.mar_nommarca, l.lin_nomlinea, ", 
      "v.cla_idclaseveh, cla.cla_nomclase,",
      "v.car_idcarroce, o.car_nomcarroce, v.col_idcolor,col_nomcolor, ",
      "v.cap_idcapaci, d.cap_nomcapaci, v.com_idcombus, b.com_nomcombus, ",
      "v.veh_motor, v.veh_serie , v.veh_modelo, v.veh_cilindraje, ",
      "v.veh_orden, v.emp_idempresa,  emp.emp_nomempresa, ",
      "v.sit_codsitio, sit_nomsitio,v.veh_numinterno, ",
      "v.est_id, t.est_descripcion, v.veh_fechafilia, v.veh_fechadesa, v.veh_codigo ",
      "FROM top_vehiculo v, top_ciudad c, top_tiposervicio s, top_linea l, ",
      "top_marca m, top_color r, top_carroceria o, top_capacidad d, ",
      "top_combustible b, top_empresa emp, top_estado t, top_clase_vehiculo cla, ",
      "top_sitio sit, top_sucursal su ",
      "WHERE v.tip_idtipo = s.tip_idtipo ",
      "AND v.suc_cod = su.suc_cod ",
      "AND v.lin_idlinea = l.lin_idlinea ",
      "AND l.mar_idmarca = m.mar_idmarca ",
      "AND d.cap_idcapaci = v.cap_idcapaci ",
      "AND o.car_idcarroce = v.car_idcarroce ", 
      "AND cla.cla_idclaseveh = v.cla_idclaseveh ",
      "AND v.sit_codsitio = sit.sit_codsitio ",
      "AND v.est_id = t.est_id ",
      "AND v.col_idcolor = r.col_idcolor ",
      "AND v.emp_idempresa = emp.emp_idempresa ",
      "AND v.com_idcombus = b.com_idcombus " ,
      "AND c.ciu_idciudad = su.ciu_idciudad ",
      "AND ", ext_query CLIPPED , 
      " AND v.suc_cod = ",g_sucursal,
      " ORDER BY v.veh_placa"
      
      PREPARE s_1 FROM mysql
      DECLARE c_1 SCROLL CURSOR FOR s_1
      LET l_total=0
      FOREACH c_1 INTO gr_vehiculo.*
        LET l_total = l_total + 1        
      END FOREACH
      MESSAGE  "Total Registros : ",l_total
      OPEN c_1
      FETCH FIRST c_1 INTO gr_vehiculo.*
      
      IF STATUS=NOTFOUND THEN
       CALL FGL_WINMESSAGE("INFO", "No hay registros !!","info")
      ELSE
        CALL array_persona()
        CALL DIALOG.setFieldActive("est_id",FALSE)
        CALL DIALOG.setActionActive("fistrow",TRUE)
        CALL DIALOG.setActionActive("nextrow",TRUE)
        CALL DIALOG.setActionActive("lastrow",TRUE)
        CALL DIALOG.setActionActive("prevrow",TRUE)
        WHENEVER ERROR CONTINUE
        FOREACH c_per INTO g_accion 
           CALL DIALOG.setActionActive(g_accion CLIPPED,TRUE)
        END FOREACH
        WHENEVER ERROR STOP 
        LET l_actual=1
        DISPLAY BY NAME gr_vehiculo.*
     
      LET imagelinea = gr_vehiculo.lin_idlinea
      DISPLAY BY NAME imagelinea
      MESSAGE  "Registro ",l_actual, " de ", l_total
    END IF
  END CONSTRUCT
     
  DISPLAY ARRAY arr_persona TO sr_persona.* 
  END DISPLAY  

  ON ACTION firstrow
    FETCH FIRST c_1 INTO gr_vehiculo.*
    LET l_actual = 1
    CALL array_persona()
    DISPLAY BY NAME gr_vehiculo.*
    LET imagelinea = gr_vehiculo.lin_idlinea
    DISPLAY BY NAME imagelinea   
    
    MESSAGE  "Registro ",l_actual, " de ", l_total
  ON ACTION lastrow
    FETCH LAST c_1 INTO gr_vehiculo.*
    LET l_actual = l_total
    CALL array_persona()
    LET imagelinea = gr_vehiculo.lin_idlinea
    DISPLAY BY NAME imagelinea
    DISPLAY BY NAME gr_vehiculo.*
    
    MESSAGE  "Registro ",l_actual, " de ", l_total
  ON ACTION nextrow
    FETCH NEXT c_1 INTO gr_vehiculo.*
   
    IF STATUS=NOTFOUND THEN
      CALL fgl_winmessage("Info","No existe mas informacion para mostrar ", "information")
    ELSE
      LET l_actual = l_actual+1
      CALL array_persona()
       LET imagelinea = gr_vehiculo.lin_idlinea
    END IF
    DISPLAY BY NAME imagelinea
    DISPLAY BY NAME gr_vehiculo.*     
    MESSAGE  "Registro ",l_actual, " de ", l_total
  ON ACTION prevrow
    FETCH PREVIOUS c_1 INTO gr_vehiculo.*
    IF STATUS = NOTFOUND THEN
      CALL fgl_winmessage("informacion","Este es el primer registro","info")
    ELSE 
      LET l_actual = l_actual-1
      CALL array_persona()
      LET imagelinea = gr_vehiculo.lin_idlinea
      DISPLAY BY NAME imagelinea
      DISPLAY BY NAME gr_vehiculo.*
      MESSAGE  "Registro ", l_actual, " de ", l_total
    END IF 
   ON ACTION modify
    CALL modificar_vehiculo() RETURNING l_bandera
     IF l_bandera THEN 
       EXIT DIALOG 
     ELSE 
       CONTINUE DIALOG 
     END IF
     
    ON ACTION eraser  
    CALL eliminar_vehiculo() RETURNING l_bandera
     IF l_bandera THEN 
       EXIT DIALOG 
     ELSE 
       CONTINUE DIALOG 
     END IF

  ON ACTION CANCEL
    CALL FGL_WINMESSAGE( "INFO", "Usuario Cancelo Consultar", "info")
    CALL fgl_settitle("Vehiculos")
    CALL arr_persona.clear()
    MESSAGE ""
    CLEAR FORM  
    EXIT DIALOG
    CLOSE c_1
    
  ON ACTION PRINTER
     MENU "Imprimir" ATTRIBUTES (STYLE="dialog",COMMENT="Desea Imprimir estos Registros ?"  )
       COMMAND "Actual"
         CALL listar_vehiculo("A")
       COMMAND "Todos" 
         CALL listar_vehiculo("T")
       COMMAND "Cancelar"
         EXIT MENU
     END MENU     
END DIALOG 
END FUNCTION                  


FUNCTION modificar_vehiculo()
DEFINE l_placa LIKE top_vehiculo.veh_placa
DEFINE l_count, l_codigo INTEGER
DEFINE l_numinterno INTEGER
DIALOG ATTRIBUTES (UNBUFFERED)                                                     
                                                                     
    INPUT BY NAME gr_vehiculo.tip_idtipo, gr_vehiculo.lin_idlinea, gr_vehiculo.cla_idclaseveh,
                  gr_vehiculo.col_idcolor, gr_vehiculo.car_idcarroce, gr_vehiculo.cap_idcapaci,
                  gr_vehiculo.com_idcombus, gr_vehiculo.veh_motor, gr_vehiculo.veh_serie,
                  gr_vehiculo.veh_modelo, gr_vehiculo.veh_cilindraje, gr_vehiculo.veh_orden,
                  gr_vehiculo.emp_idempresa, gr_vehiculo.sit_codsitio,
                  gr_vehiculo.veh_numinterno ATTRIBUTES(WITHOUT DEFAULTS)

    BEFORE INPUT
     LET l_numinterno = gr_vehiculo.veh_numinterno 
     SELECT est_descripcion
     INTO gr_vehiculo.est_descripcion
     FROM top_estado 
     WHERE est_id = gr_vehiculo.est_id
     DISPLAY BY NAME gr_vehiculo.est_descripcion
      CALL fgl_settitle("Modificar Vehiculos")
      ON ACTION zoom
        CASE 
         WHEN INFIELD(tip_idtipo)
          LET l_codigo = gr_vehiculo.tip_idtipo  
          CALL make_help("top_tiposervicio","tip_idtipo", "tip_nomtipo", "TIPO SERVICIO", null) RETURNING gr_vehiculo.tip_idtipo
          CLOSE WINDOW w_help
          IF gr_vehiculo.tip_idtipo IS NOT NULL THEN 
           SELECT tip_nomtipo  INTO gr_vehiculo.tip_nomtipo FROM top_tiposervicio
           WHERE gr_vehiculo.tip_idtipo=top_tiposervicio.tip_idtipo
           DISPLAY BY NAME gr_vehiculo.tip_idtipo, gr_vehiculo.tip_nomtipo
          ELSE
           SELECT tip_idtipo,tip_nomtipo INTO gr_vehiculo.tip_idtipo,gr_vehiculo.tip_nomtipo 
           FROM top_tiposervicio
           WHERE top_tiposervicio.tip_idtipo = l_codigo
           DISPLAY BY NAME gr_vehiculo.tip_idtipo, gr_vehiculo.tip_nomtipo 
          END IF  
             
          WHEN INFIELD(lin_idlinea) 
           LET l_codigo = gr_vehiculo.lin_idlinea
           CALL make() RETURNING gr_vehiculo.lin_idlinea
           CLOSE WINDOW w_help
           IF gr_vehiculo.lin_idlinea > 0 THEN 
            SELECT top_marca.mar_nommarca, top_linea.lin_idlinea, top_linea.lin_nomlinea
            INTO gr_vehiculo.mar_nommarca, gr_vehiculo.lin_idlinea, gr_vehiculo.lin_nomlinea
            FROM top_marca, top_linea
            WHERE top_linea.lin_idlinea=gr_vehiculo.lin_idlinea
            AND  top_marca.mar_idmarca=top_linea.mar_idmarca 
           ELSE 
            SELECT top_marca.mar_nommarca, top_linea.lin_idlinea, top_linea.lin_nomlinea
            INTO gr_vehiculo.mar_nommarca, gr_vehiculo.lin_idlinea, gr_vehiculo.lin_nomlinea
            FROM top_marca, top_linea
            WHERE top_linea.lin_idlinea = l_codigo
            AND top_marca.mar_idmarca = top_linea.mar_idmarca
            DISPLAY BY NAME gr_vehiculo.lin_idlinea, gr_vehiculo.lin_nomlinea
           END IF
              
           WHEN INFIELD(cla_idclaseveh) 
            LET l_codigo = gr_vehiculo.lin_idlinea
            CALL make_help("top_clase_vehiculo","cla_idclaseveh", "cla_nomclase", "CLASE DE SERVICIO", null) RETURNING gr_vehiculo.cla_idclaseveh             
            CLOSE WINDOW w_help
            IF gr_vehiculo.cla_idclaseveh IS NOT NULL THEN 
             SELECT cla_nomclase INTO gr_vehiculo.cla_nomclase 
             FROM top_clase_vehiculo
             WHERE top_clase_vehiculo.cla_idclaseveh = gr_vehiculo.lin_idlinea 
             DISPLAY BY NAME gr_vehiculo.cla_idclaseveh, gr_vehiculo.cla_nomclase
            ELSE 
             SELECT cla_idclaseveh,cla_nomclase INTO gr_vehiculo.lin_idlinea,gr_vehiculo.cla_nomclase 
             FROM top_clase_vehiculo
             WHERE top_clase_vehiculo.cla_idclaseveh = l_codigo
             DISPLAY BY NAME gr_vehiculo.cla_idclaseveh, gr_vehiculo.cla_nomclase
            END IF
             
            WHEN INFIELD(car_idcarroce)
             LET l_codigo = gr_vehiculo.car_idcarroce 
             CALL make_help("top_carroceria","car_idcarroce", "car_nomcarroce", "CARROCERIAS", null) RETURNING gr_vehiculo.car_idcarroce
             CLOSE WINDOW w_help
             IF gr_vehiculo.car_idcarroce IS NOT NULL THEN 
              SELECT top_carroceria.car_nomcarroce  INTO gr_vehiculo.car_nomcarroce FROM top_carroceria
              WHERE gr_vehiculo.car_idcarroce=top_carroceria.car_idcarroce
              DISPLAY BY NAME gr_vehiculo.car_idcarroce, gr_vehiculo.car_nomcarroce
             ELSE 
              SELECT car_idcarroce,car_nomcarroce  INTO gr_vehiculo.car_idcarroce,gr_vehiculo.car_nomcarroce 
              FROM top_carroceria
              WHERE top_carroceria.car_idcarroce = l_codigo
              DISPLAY BY NAME gr_vehiculo.car_idcarroce, gr_vehiculo.car_nomcarroce
             END IF 

            WHEN INFIELD(col_idcolor)
             LET l_codigo = gr_vehiculo.col_idcolor 
             CALL make_help("top_color","col_idcolor", "col_nomcolor", "COLORES", null) RETURNING gr_vehiculo.col_idcolor
             CLOSE WINDOW w_help
             IF gr_vehiculo.col_idcolor IS NOT NULL THEN 
              SELECT top_color.col_nomcolor  INTO gr_vehiculo.col_nomcolor FROM top_color
              WHERE gr_vehiculo.col_idcolor=top_color.col_idcolor
              DISPLAY BY NAME gr_vehiculo.col_idcolor, gr_vehiculo.col_nomcolor
             ELSE
              SELECT col_idcolor,col_nomcolor INTO gr_vehiculo.col_idcolor, gr_vehiculo.col_nomcolor 
              FROM top_color
              WHERE top_color.col_idcolor = l_codigo 
              DISPLAY BY NAME gr_vehiculo.col_idcolor, gr_vehiculo.col_nomcolor
             END IF 
               
            WHEN INFIELD(cap_idcapaci)
             LET l_codigo = gr_vehiculo.cap_idcapaci 
             CALL make_help("top_capacidad","cap_idcapaci", "cap_nomcapaci", "CAPACIDAD", null) RETURNING gr_vehiculo.cap_idcapaci
             CLOSE WINDOW w_help
             IF gr_vehiculo.cap_idcapaci IS NOT NULL THEN  
              SELECT top_capacidad.cap_nomcapaci  INTO gr_vehiculo.cap_nomcapaci FROM top_capacidad
              WHERE gr_vehiculo.cap_idcapaci=top_capacidad.cap_idcapaci
              DISPLAY BY NAME gr_vehiculo.cap_idcapaci, gr_vehiculo.cap_nomcapaci
             ELSE 
              SELECT cap_idcapaci,cap_nomcapaci INTO gr_vehiculo.cap_idcapaci,gr_vehiculo.cap_nomcapaci 
              FROM top_capacidad
              WHERE top_capacidad.cap_idcapaci = l_codigo
              DISPLAY BY NAME gr_vehiculo.cap_idcapaci, gr_vehiculo.cap_nomcapaci
             END IF 
              
            WHEN INFIELD(com_idcombus) 
             LET l_codigo = gr_vehiculo.com_idcombus
             CALL make_help("top_combustible","com_idcombus", "com_nomcombus", "COMBUSTIBLE", null) RETURNING gr_vehiculo.com_idcombus
             CLOSE WINDOW w_help
             IF gr_vehiculo.com_idcombus IS NOT NULL THEN 
              SELECT com_nomcombus  INTO gr_vehiculo.com_nomcombus FROM top_combustible
              WHERE gr_vehiculo.com_idcombus=top_combustible.com_idcombus
              DISPLAY BY NAME gr_vehiculo.com_idcombus, gr_vehiculo.com_nomcombus
             ELSE 
              SELECT com_idcombus,com_nomcombus  INTO gr_vehiculo.com_idcombus,gr_vehiculo.com_nomcombus 
              FROM top_combustible
              WHERE top_combustible.com_idcombus = l_codigo
              DISPLAY BY NAME gr_vehiculo.com_idcombus, gr_vehiculo.com_nomcombus
             END IF  
             
            WHEN INFIELD(emp_idempresa) 
             CALL make3() RETURNING gr_vehiculo.emp_idempresa
             CLOSE WINDOW w_help4
             SELECT emp_nomempresa INTO gr_vehiculo.emp_nomempresa
             FROM top_empresa 
             WHERE suc_cod = g_sucursal AND tip_idtipoem = 1 
             AND emp_idempresa =  gr_vehiculo.emp_idempresa
             DISPLAY BY NAME gr_vehiculo.emp_idempresa, gr_vehiculo.emp_nomempresa

            WHEN INFIELD(sit_codsitio) 
             LET l_codigo = gr_vehiculo.sit_codsitio
             CALL make_help("top_sitio","sit_codsitio", "sit_nomsitio", "SITIOS", null) RETURNING gr_vehiculo.sit_codsitio
             CLOSE WINDOW w_help
             IF gr_vehiculo.sit_codsitio IS NOT NULL THEN 
              SELECT top_sitio.sit_nomsitio  INTO gr_vehiculo.sit_nomsitio FROM top_sitio
              WHERE top_sitio.sit_codsitio=gr_vehiculo.sit_codsitio
              DISPLAY BY NAME gr_vehiculo.sit_codsitio, gr_vehiculo.sit_nomsitio
             ELSE 
              SELECT sit_codsitio,sit_nomsitio  INTO gr_vehiculo.sit_codsitio,gr_vehiculo.sit_nomsitio 
              FROM top_sitio
              WHERE top_sitio.sit_codsitio = l_codigo
              DISPLAY BY NAME gr_vehiculo.sit_codsitio, gr_vehiculo.sit_nomsitio
             END IF 
              
          END CASE
          
       AFTER FIELD tip_idtipo
        IF gr_vehiculo.tip_idtipo IS NOT NULL THEN
         SELECT tip_nomtipo  INTO gr_vehiculo.tip_nomtipo FROM top_tiposervicio
         WHERE gr_vehiculo.tip_idtipo=top_tiposervicio.tip_idtipo
         IF sqlca.sqlcode <> 0 THEN 
          CALL fgl_winmessage("Info","Ese codigo de Tipo de Servicio no existe", "information")
          NEXT FIELD tip_idtipo
         ELSE 
          DISPLAY BY NAME gr_vehiculo.tip_idtipo, gr_vehiculo.tip_nomtipo
         END IF
        END IF
       
       AFTER FIELD lin_idlinea 
        IF gr_vehiculo.lin_idlinea IS NOT NULL THEN
         SELECT  top_marca.mar_nommarca, top_linea.lin_idlinea, top_linea.lin_nomlinea
         INTO gr_vehiculo.mar_nommarca, gr_vehiculo.lin_idlinea,  gr_vehiculo.lin_nomlinea
         FROM top_marca, top_linea
         WHERE top_linea.lin_idlinea=gr_vehiculo.lin_idlinea
         AND top_marca.mar_idmarca=top_linea.mar_idmarca 
         IF sqlca.sqlcode <> 0 THEN 
          CALL fgl_winmessage("Info","Ese codigo de Marca  no existe", "information")
          NEXT FIELD lin_idlinea
         ELSE
          DISPLAY BY NAME gr_vehiculo.lin_idlinea, gr_vehiculo.mar_nommarca, gr_vehiculo.lin_nomlinea
         END IF
        END IF
           
       AFTER FIELD col_idcolor 
        IF gr_vehiculo.col_idcolor IS NOT NULL THEN
         SELECT top_color.col_nomcolor  INTO gr_vehiculo.col_nomcolor FROM top_color
         WHERE gr_vehiculo.col_idcolor=top_color.col_idcolor
         IF sqlca.sqlcode <> 0 THEN 
          CALL fgl_winmessage("Info","Ese codigo de Color no existe", "information")
          NEXT FIELD col_idcolor
         ELSE 
          DISPLAY BY NAME gr_vehiculo.col_idcolor, gr_vehiculo.col_nomcolor
         END IF
        END IF
         
       AFTER FIELD car_idcarroce
        IF gr_vehiculo.car_idcarroce IS NOT NULL THEN
         SELECT top_carroceria.car_nomcarroce  INTO gr_vehiculo.car_nomcarroce FROM top_carroceria
         WHERE gr_vehiculo.car_idcarroce=top_carroceria.car_idcarroce
         IF sqlca.sqlcode <> 0 THEN 
          CALL fgl_winmessage("Info","Ese codigo de Carroceria no existe", "information")
          NEXT FIELD car_idcarroce
         ELSE 
          DISPLAY BY NAME gr_vehiculo.car_idcarroce, gr_vehiculo.car_nomcarroce
         END IF
        END IF

       AFTER FIELD cla_idclaseveh
        IF gr_vehiculo.cla_idclaseveh IS NOT NULL THEN 
         SELECT top_clase_vehiculo.cla_nomclase  INTO gr_vehiculo.cla_nomclase FROM top_clase_vehiculo
         WHERE gr_vehiculo.cla_idclaseveh=top_clase_vehiculo.cla_idclaseveh
         IF sqlca.sqlcode <> 0 THEN 
          CALL fgl_winmessage("Info","Ese codigo de Clase de Vehiculo NO existe", "information")
          LET gr_vehiculo.cla_nomclase = NULL
          NEXT FIELD cla_idclaseveh
         ELSE 
          DISPLAY BY NAME gr_vehiculo.car_idcarroce, gr_vehiculo.car_nomcarroce
         END IF
        END IF 
         
       AFTER FIELD cap_idcapaci
         IF gr_vehiculo.cap_idcapaci IS NOT NULL THEN
          SELECT top_capacidad.cap_nomcapaci  INTO gr_vehiculo.cap_nomcapaci FROM top_capacidad
          WHERE gr_vehiculo.cap_idcapaci=top_capacidad.cap_idcapaci
          IF sqlca.sqlcode <> 0 THEN 
           CALL fgl_winmessage("Info","Ese codigo de Capacidad no existe", "information")
           NEXT FIELD cap_idcapaci
          ELSE
           DISPLAY BY NAME gr_vehiculo.cap_idcapaci, gr_vehiculo.cap_nomcapaci
          END IF 
         END IF
               
       AFTER FIELD com_idcombus
         IF gr_vehiculo.com_idcombus IS NOT NULL THEN
          SELECT top_combustible.com_nomcombus  INTO gr_vehiculo.com_nomcombus FROM top_combustible
          WHERE gr_vehiculo.com_idcombus=top_combustible.com_idcombus
           IF sqlca.sqlcode <> 0 THEN 
             CALL fgl_winmessage("Info","Ese codigo de Combustible no existe", "information")
             NEXT FIELD  com_idcombus
           ELSE 
             DISPLAY BY NAME gr_vehiculo.com_idcombus, gr_vehiculo.com_nomcombus
           END IF
         END IF
                   
       AFTER FIELD veh_motor
          IF gr_vehiculo.veh_motor IS NOT NULL THEN
            CALL existe_dato ("top_vehiculo","veh_motor", gr_vehiculo.veh_motor) RETURNING g_existe_dato
            IF g_existe_dato THEN
              SELECT top_vehiculo.veh_placa INTO l_placa 
              FROM top_vehiculo 
              WHERE gr_vehiculo.veh_motor = top_vehiculo.veh_motor
              IF l_placa<>gr_vehiculo.veh_placa THEN
                CALL fgl_winmessage("Info","Motor ya Existe", "information")
                NEXT FIELD  veh_motor
             END IF 
            END IF
          END IF
        
      AFTER FIELD veh_serie
          IF gr_vehiculo.veh_serie IS NOT NULL THEN
            CALL existe_dato ("top_vehiculo","veh_serie", gr_vehiculo.veh_serie) RETURNING g_existe_dato
            IF g_existe_dato THEN
              SELECT top_vehiculo.veh_placa INTO l_placa 
              FROM top_vehiculo 
              WHERE gr_vehiculo.veh_serie = top_vehiculo.veh_serie
              IF l_placa<>gr_vehiculo.veh_placa THEN
                CALL fgl_winmessage("Info","Numero de serie  ya existe", "information")
                NEXT FIELD veh_serie
              END IF
            END IF
          END IF

      AFTER FIELD veh_modelo
        IF gr_vehiculo.veh_modelo IS NOT NULL THEN
         IF gr_vehiculo.veh_modelo<1990 OR gr_vehiculo.veh_modelo>YEAR(CURRENT)+ 1 THEN
             CALL fgl_winmessage("Info","Modelo NO Válido", "information")
          NEXT FIELD veh_modelo
         END IF
        END IF
         
         AFTER FIELD veh_cilindraje
         IF gr_vehiculo.veh_cilindraje IS NOT NULL THEN
           IF gr_vehiculo.veh_cilindraje <80 OR gr_vehiculo.veh_cilindraje>7000 THEN
             CALL fgl_winmessage("Info","Cilindraje NO Válido", "information")
              NEXT FIELD veh_cilindraje
           END IF
         END IF
         
         AFTER FIELD veh_orden
          IF gr_vehiculo.veh_orden IS NOT NULL THEN
            CALL existe_orden (g_sucursal, gr_vehiculo.veh_orden) RETURNING g_existe_orden
            IF g_existe_orden THEN
              SELECT top_vehiculo.veh_placa INTO l_placa 
              FROM top_vehiculo 
              WHERE top_vehiculo.veh_orden = gr_vehiculo.veh_orden  
              IF l_placa <> gr_vehiculo.veh_placa THEN
                CALL fgl_winmessage("Info","Numero de orden ya asignado", "information")
                NEXT FIELD veh_orden
              END IF
            END IF
          END IF
          
         AFTER FIELD emp_idempresa
          SELECT top_empresa.emp_nomempresa  INTO gr_vehiculo.emp_nomempresa FROM top_empresa
          WHERE suc_cod = g_sucursal AND tip_idtipoem = 1
          AND emp_idempresa = gr_vehiculo.emp_idempresa
          IF sqlca.sqlcode <> 0 THEN 
           CALL fgl_winmessage("Info","Ese codigo de Empresa no existe", "information")
           NEXT FIELD emp_idempresa
          ELSE
           DISPLAY BY NAME gr_vehiculo.emp_idempresa, gr_vehiculo.emp_nomempresa
         END IF
              
         AFTER FIELD sit_codsitio
          IF gr_vehiculo.sit_codsitio IS NOT NULL THEN
           SELECT top_sitio.sit_nomsitio INTO gr_vehiculo.sit_nomsitio FROM top_sitio
           WHERE top_sitio.sit_codsitio=gr_vehiculo.sit_codsitio
           IF STATUS = NOTFOUND THEN 
            CALL fgl_winmessage("Info","Ese codigo de Sede NO existe", "information")
            LET gr_vehiculo.sit_nomsitio = NULL
            NEXT FIELD sit_codsitio
           ELSE
            CALL DIALOG.setFieldActive("veh_numinterno",TRUE)
            DISPLAY BY NAME gr_vehiculo.sit_codsitio, gr_vehiculo.sit_nomsitio
            NEXT FIELD tip_idtipo
           END IF
          END IF

     END INPUT

    ON ACTION CANCEL 
     CALL FGL_WINMESSAGE( "INFO", "Usuario Cancelo Modificar", "info")
     CALL fgl_settitle("Consultar Vehiculos")
     CLEAR FORM
     RETURN FALSE 
  
    ON ACTION save 
     
     IF gr_vehiculo.tip_idtipo IS NULL THEN
      CALL fgl_winmessage("Info","Complete Tipo de Servicio  ", "information")
      NEXT FIELD tip_idtipo
     END IF

     IF gr_vehiculo.lin_idlinea IS NULL THEN
      CALL fgl_winmessage("Info","Complete Marca y Linea  ", "information")
      NEXT FIELD lin_idlinea
     END IF

     IF gr_vehiculo.col_idcolor IS NULL THEN
      CALL fgl_winmessage("Info","Complete Color  ", "information")
      NEXT FIELD col_idcolor
     END IF

     IF gr_vehiculo.car_idcarroce IS NULL THEN
      CALL fgl_winmessage("Info","Complete Carroceria  ", "information")
      NEXT FIELD car_idcarroce
     END IF
       
     IF gr_vehiculo.cap_idcapaci IS NULL THEN
      CALL fgl_winmessage("Info","Complete Carroceria  ", "information")
      NEXT FIELD cap_idcapaci
     END IF
       
     IF gr_vehiculo.com_idcombus IS NULL THEN
      CALL fgl_winmessage("Info","Complete Tipo Combustible  ", "information")
      NEXT FIELD com_idcombus
     END IF

     IF gr_vehiculo.veh_motor IS NULL THEN
      CALL fgl_winmessage("Info","Complete N. Motor  ", "information")
      NEXT FIELD veh_motor
     ELSE
      CALL existe_dato ("top_vehiculo","veh_motor", gr_vehiculo.veh_motor) RETURNING g_existe_dato
      IF g_existe_dato THEN
       SELECT top_vehiculo.veh_placa INTO l_placa 
       FROM top_vehiculo 
       WHERE gr_vehiculo.veh_motor = top_vehiculo.veh_motor
       IF l_placa<>gr_vehiculo.veh_placa THEN
        CALL fgl_winmessage("Info","Motor ya Existe", "information")
        NEXT FIELD veh_motor
       END IF 
      END IF 
     END IF
       
     IF gr_vehiculo.veh_serie IS NULL THEN
      CALL fgl_winmessage("Info","Complete  Serie  ", "information")
      NEXT FIELD veh_serie
     ELSE
      CALL existe_dato ("top_vehiculo","veh_serie", gr_vehiculo.veh_serie) RETURNING g_existe_dato
      IF g_existe_dato THEN
       SELECT top_vehiculo.veh_placa INTO l_placa 
       FROM top_vehiculo 
       WHERE gr_vehiculo.veh_serie = top_vehiculo.veh_serie
       IF l_placa<>gr_vehiculo.veh_placa THEN
        CALL fgl_winmessage("Info","Numero de serie  ya existe", "information")
        NEXT FIELD veh_serie
       END IF
      END IF 
     END IF

     IF gr_vehiculo.veh_modelo IS NULL THEN
      CALL fgl_winmessage("Info","Complete  Modelo  ", "information")
      NEXT FIELD veh_modelo
     END IF

     IF gr_vehiculo.veh_cilindraje IS NULL THEN
      CALL fgl_winmessage("Info","Complete Cilindraje  ", "information")
      NEXT FIELD veh_cilindraje
     END IF

     IF gr_vehiculo.veh_orden IS NULL THEN
      CALL fgl_winmessage("Info","Complete N. Orden  ", "information")
      NEXT FIELD veh_orden
     ELSE
      CALL existe_orden (g_sucursal, gr_vehiculo.veh_orden) RETURNING g_existe_orden
      IF g_existe_orden THEN
       SELECT top_vehiculo.veh_placa INTO l_placa 
       FROM top_vehiculo 
       WHERE gr_vehiculo.veh_orden = top_vehiculo.veh_orden
       IF l_placa <> gr_vehiculo.veh_placa THEN
        CALL fgl_winmessage("Info","Numero de orden ya asignado", "information")
        NEXT FIELD veh_orden
       END IF
      END IF
     END IF

     IF gr_vehiculo.emp_idempresa IS NULL THEN
      CALL fgl_winmessage("Info","Complete Empre. Afiliadora  ", "information")
      NEXT FIELD emp_nitempresa
     END IF

     IF gr_vehiculo.sit_codsitio <> 1 THEN
      IF gr_vehiculo.veh_numinterno <> 0 THEN
       IF l_numinterno <> gr_vehiculo.veh_numinterno THEN 
        SELECT COUNT(*)
        INTO l_count  
        FROM top_vehiculo
        WHERE sit_codsitio = gr_vehiculo.sit_codsitio
        AND veh_numinterno = gr_vehiculo.veh_numinterno
        AND suc_cod = g_sucursal
        IF l_count >= 1 THEN
         CALL fgl_winmessage("Info","Numero Interno ya existe", "information")
         NEXT FIELD veh_numinterno
        ELSE
         DISPLAY gr_vehiculo.veh_numinterno
        END IF         
       END IF
      END IF   
     ELSE 
      CALL fgl_winmessage("Info","Numero Interno no puede ser cero", "information")
      NEXT FIELD veh_numinterno
     END IF  
     
   BEGIN WORK
    WHENEVER ERROR CONTINUE 
    UPDATE  top_vehiculo SET tip_idtipo=gr_vehiculo.tip_idtipo, 
                                lin_idlinea=gr_vehiculo.lin_idlinea, col_idcolor=gr_vehiculo.col_idcolor, 
                                car_idcarroce=gr_vehiculo.car_idcarroce, cap_idcapaci=gr_vehiculo.cap_idcapaci,
                                cla_idclaseveh=gr_vehiculo.cla_idclaseveh,
                                com_idcombus=gr_vehiculo.com_idcombus, veh_motor=gr_vehiculo.veh_motor, 
                                veh_serie=gr_vehiculo.veh_serie, veh_modelo=gr_vehiculo.veh_modelo,  
                                veh_cilindraje=gr_vehiculo.veh_cilindraje,   veh_orden=gr_vehiculo.veh_orden,
                                est_id=gr_vehiculo.est_id,  emp_idempresa=gr_vehiculo.emp_idempresa, 
                                sit_codsitio=gr_vehiculo.sit_codsitio, veh_numinterno = gr_vehiculo.veh_numinterno,
                                veh_fechactual=CURRENT YEAR TO SECOND , veh_usuario=g_usuario
    WHERE veh_placa=gr_vehiculo.veh_placa
    IF SQLCA.SQLCODE<>0 THEN
     CALL FGL_WINMESSAGE( "STOP", "NO SE PUEDE ACTUALIZAR.....COMUNIQUESE CON TECNOLOGIA", "error")
     ERROR SQLCA.SQLCODE||' '||SQLERRMESSAGE
     ROLLBACK WORK
    ELSE 
     CALL FGL_WINMESSAGE( "INFO", "VEHICULO ACTUALIZADO", "info")
     COMMIT WORK
     RETURN TRUE
    END IF

END DIALOG 
    
END FUNCTION


FUNCTION eliminar_vehiculo()
RETURN TRUE
END FUNCTION


FUNCTION array_persona()
INITIALIZE arr_persona TO NULL 
  DECLARE c_2 CURSOR FOR
   SELECT top_persona.per_numidenti, top_persona.per_nombre, 
   top_tipopersona.tip_nomtipoper, top_estado.est_descripcion,
   top_persovehiculo.per_fechavincula
   FROM top_vehiculo, top_persona, top_tipopersona,top_persovehiculo,
   top_estado
   WHERE top_vehiculo.veh_codigo = top_persovehiculo.veh_codigo
   AND   top_persona.per_numidenti  = top_persovehiculo.per_numidenti
   AND   top_persovehiculo.tip_idtipoper=top_tipopersona.tip_idtipoper
   AND   top_persona.est_idestaper=top_estado.est_id
   AND   top_vehiculo.veh_placa=gr_vehiculo.veh_placa
   AND   top_persona.suc_cod = g_sucursal
 
   FOREACH c_2 INTO gr_persona.*
     CALL arr_persona.appendElement()
     LET arr_persona[arr_persona.getLength()].*=gr_persona.*
   END FOREACH
END FUNCTION



FUNCTION listar_vehiculo(opcion)
DEFINE handler om.SaxDocumentHandler
DEFINE opcion CHAR(1) 

IF fgl_report_loadCurrentSettings(NULL) THEN        -- switch on Compatibility mode (run without .4rp file)
  LET handler = configureOutput()  -- commit settings
END IF  

START REPORT reporte_vehiculo TO XML HANDLER handler
IF opcion = "A" THEN
  OUTPUT TO REPORT reporte_vehiculo  (gr_vehiculo.*)
ELSE 
  FOREACH c_1 INTO gr_vehiculo.*  
    OUTPUT TO REPORT reporte_vehiculo (gr_vehiculo.*)    
  END FOREACH
END IF  
FINISH REPORT reporte_vehiculo
END FUNCTION 


REPORT reporte_vehiculo(lr_x)
DEFINE lr_x            RECORD 
    veh_placa            LIKE    top_vehiculo.veh_placa,
    ciu_idciudad         LIKE    top_ciudad.ciu_idciudad,
    ciu_nomciudad        LIKE    top_ciudad.ciu_nomciudad, 
    tip_idtipo           LIKE    top_vehiculo.tip_idtipo,   
    tip_nomtipo          LIKE    top_tiposervicio.tip_nomtipo,
    lin_idlinea          LIKE    top_vehiculo.lin_idlinea, 
    mar_nommarca         LIKE    top_marca.mar_nommarca,
    lin_nomlinea         LIKE    top_linea.lin_nomlinea, 
    cla_idclaseveh       LIKE    top_vehiculo.cla_idclaseveh,
    cla_nomclase         LIKE    top_clase_vehiculo.cla_nomclase,
    car_idcarroce        LIKE    top_vehiculo.car_idcarroce,
    car_nomcarroce       LIKE    top_carroceria.car_nomcarroce,
    col_idcolor          LIKE    top_vehiculo.col_idcolor,
    col_nomcolor         LIKE    top_color.col_nomcolor, 
    cap_idcapaci         LIKE    top_vehiculo.cap_idcapaci, 
    cap_nomcapaci        LIKE    top_capacidad.cap_nomcapaci, 
    com_idcombus         LIKE    top_vehiculo.com_idcombus, 
    com_nomcombus        LIKE    top_combustible.com_nomcombus, 
    veh_motor            LIKE    top_vehiculo.veh_motor, 
    veh_serie            LIKE    top_vehiculo.veh_serie, 
    veh_modelo           LIKE    top_vehiculo.veh_modelo, 
    veh_cilindraje       LIKE    top_vehiculo.veh_cilindraje, 
    veh_orden            LIKE    top_vehiculo.veh_orden,
    emp_idempresa        LIKE    top_vehiculo.emp_idempresa,
    emp_nomempresa       LIKE    top_empresa.emp_nomempresa,
    sit_codsitio         LIKE    top_vehiculo.sit_codsitio,
    sit_nomsitio         LIKE    top_sitio.sit_nomsitio,
    veh_numinterno       LIKE    top_vehiculo.veh_numinterno,
    est_id               LIKE    top_vehiculo.est_id,
    est_descripcion      LIKE    top_estado.est_descripcion,
    veh_fechafilia       LIKE    top_vehiculo.veh_fechafilia,
    veh_fechadesa        LIKE    top_vehiculo.veh_fechadesa,
    per_tipoidenti       LIKE    top_persona.per_tipoidenti,
    per_numidenti        LIKE    top_persona.per_numidenti,
    per_nombre           LIKE    top_persona.per_nombre,
    veh_codigo           LIKE    top_vehiculo.veh_codigo END RECORD
    DEFINE  l_tplaca    LIKE  top_vehiculo.veh_placa,
            l_tciudad   LIKE  top_ciudad.ciu_nomciudad,
            l_tmarca    LIKE  top_marca.mar_nommarca,
            l_tmodelo   CHAR(6),
            l_tempresa  LIKE top_empresa.emp_nomempresa,
            l_testado   LIKE top_estado.est_descripcion,
            l_fechafi   CHAR(20),
            l_lineas    CHAR(200)            
     
FORMAT
  PAGE HEADER
    LET l_tplaca   = "PLACA"
    LET l_tciudad  = "CIUDAD"
    LET l_tmarca   = "MARCA"
    LET l_tmodelo  = "MODELO"
    LET l_tempresa = "EMPRESA"
    LET l_testado  = "ESTADO"
    LET l_fechafi  = "FECHA AFILIACION"
    LET l_lineas   = "___________________________________________________________________________________________________________________________________________________________________________"
    
    PRINT  COLUMN 1, l_lineas 
    PRINT  COLUMN 1 ,l_tplaca, COLUMN 12, l_tciudad, COLUMN 28 , l_tmarca,COLUMN 52 , l_tmodelo, COLUMN 60 , l_tempresa, COLUMN 111 , l_testado, COLUMN 126 , l_fechafi
    PRINT  COLUMN 1, l_lineas
    
  ON EVERY ROW
    PRINT COLUMN 1 ,lr_x.veh_placa, COLUMN 12 ,lr_x.ciu_nomciudad, COLUMN 28 , lr_x.mar_nommarca CLIPPED, 
          COLUMN 50,lr_x.veh_modelo CLIPPED , COLUMN 60 , lr_x.emp_nomempresa , 
          COLUMN 111, lr_x.est_descripcion , COLUMN  126, lr_x.veh_fechafilia
    
END REPORT


FUNCTION make()
DEFINE lr_help RECORD
       codigo INTEGER,
       descr  varchar(80),
       nomb varchar(80)
END RECORD
DEFINE l_where_info STRING
DEFINE l_titulo,l_query STRING
DEFINE l_arr_help DYNAMIC ARRAY OF RECORD
    codigo  INTEGER,
    descr   varchar(80),
    nomb    varchar(80)
    END RECORD
DEFINE lr_filtro RECORD
       campo    varchar(20),
       filtro   varchar(80)
END RECORD

LET l_titulo = 'MARCAS'
OPEN WINDOW w_help WITH FORM "fhelp2_1"
CALL fgl_settitle(l_titulo)

DIALOG ATTRIBUTES(UNBUFFERED, FIELD ORDER FORM)
  INPUT BY NAME lr_filtro.*
  
    BEFORE INPUT
      LET lr_filtro.campo="LINEA"
      DISPLAY BY NAME lr_filtro.campo
      CALL l_arr_help.clear()
      LET l_query="SELECT l.lin_idlinea,m.mar_nommarca,l.lin_nomlinea
                   FROM top_linea l, top_marca m
                   WHERE l.mar_idmarca = m.mar_idmarca"

PREPARE ext_sql1 FROM l_query
      DECLARE lc_help1 CURSOR FOR ext_sql1
      CALL l_arr_help.clear()

      FOREACH lc_help1 INTO lr_help.*
        CALL l_arr_help.appendElement()
        LET l_arr_help[l_arr_help.getLength()].* = lr_help.*
      END FOREACH
    
     AFTER FIELD filtro
      
       IF lr_filtro.campo="LINEA" THEN 
         LET l_where_info = 'l.lin_nomlinea'

         LET l_query = "SELECT l.lin_idlinea,m.mar_nommarca,l.lin_nomlinea ",
                    "FROM top_linea l, top_marca m ",
                    "WHERE l.mar_idmarca = m.mar_idmarca ",
                    "AND ",l_where_info ," matches '*", 
                    lr_filtro.filtro CLIPPED,"*'"
       END IF 

       IF lr_filtro.campo="MARCA" THEN 
         LET l_where_info = 'm.mar_nommarca' 

         LET l_query = "SELECT l.lin_idlinea,m.mar_nommarca,l.lin_nomlinea ",
                    "FROM top_linea l, top_marca m ",
                    "WHERE l.mar_idmarca = m.mar_idmarca ",
                    "AND ",l_where_info ," matches '*", 
                    lr_filtro.filtro CLIPPED,"*'"
       END IF 

       IF lr_filtro.campo = ""  OR lr_filtro.campo IS NULL THEN  
         LET l_query="SELECT l.lin_idlinea,m.mar_nommarca,l.lin_nomlinea
                   FROM top_linea l, top_marca m
                   WHERE l.mar_idmarca = m.mar_idmarca"
       END IF   
  
      PREPARE ext_sql2 FROM l_query
      DECLARE lc_help2 CURSOR FOR ext_sql2
      CALL l_arr_help.clear()

      FOREACH lc_help2 INTO lr_help.*
        CALL l_arr_help.appendElement()
        LET l_arr_help[l_arr_help.getLength()].* = lr_help.*
      END FOREACH

      IF l_arr_help.getLength() = 0 THEN
        CALL fgl_winmessage("Consulta","No existen Registros para ese Parametro!", "information")
      END IF
   
  END INPUT
  DISPLAY ARRAY l_arr_help TO table2.*
    ON ACTION ACCEPT
      RETURN l_arr_help[arr_curr()].codigo
  END DISPLAY
  ON ACTION CANCEL
    RETURN 0
  BEFORE DIALOG
    NEXT FIELD campo
END DIALOG

END FUNCTION


FUNCTION make3()

DEFINE lr_help RECORD
       codigo INTEGER,
       descr  varchar(80)
END RECORD

DEFINE l_titulo, le_query STRING
DEFINE l_arr_help DYNAMIC ARRAY OF RECORD
    codigo  INTEGER,
    descr   varchar(80)
    END RECORD
DEFINE lr_filtro RECORD
       campo    varchar(20),
       filtro   varchar(80)
END RECORD
LET l_titulo = 'EMPRESAS'
INITIALIZE lr_help.* TO NULL
CALL l_arr_help.clear()
OPEN WINDOW w_help4 WITH FORM "fhelp2"
CALL fgl_settitle(l_titulo)

DIALOG ATTRIBUTES(UNBUFFERED, FIELD ORDER FORM)
  INPUT BY NAME lr_filtro.*
  
    BEFORE INPUT
      LET lr_filtro.campo = "CODIGO"
      DISPLAY BY NAME lr_filtro.campo
      CALL l_arr_help.clear()
      LET le_query = "SELECT emp_idempresa, emp_nomempresa ",
                     "FROM top_empresa ",
                     "WHERE suc_cod = ",g_sucursal CLIPPED,
                     " AND tip_idtipoem = 1"
   
      PREPARE squery FROM le_query
      DECLARE lc_query CURSOR FOR squery
      CALL l_arr_help.clear()

      FOREACH lc_query INTO lr_help.*
        CALL l_arr_help.appendElement()
        LET l_arr_help[l_arr_help.getLength()].* = lr_help.*
      END FOREACH
    
    AFTER FIELD filtro
      
      IF lr_filtro.campo = "CODIGO" THEN 

        LET le_query = "SELECT emp_idempresa, emp_nomempresa ",
                       "FROM top_empresa ",
                       "WHERE suc_cod = ",g_sucursal CLIPPED,
                       " AND tip_idtipoem = 1 AND emp_nitempresa = ",
                       lr_filtro.filtro CLIPPED
      END IF 

      IF lr_filtro.campo = "DESCRIPCION" THEN 
    
        LET le_query = "SELECT emp_idempresa, emp_nomempresa ",
                       "FROM top_empresa ",
                       "WHERE suc_cod = ",g_sucursal CLIPPED,
                       " AND tip_idtipoem = 1 AND emp_nomempresa ",
                       " matches '*", lr_filtro.filtro CLIPPED,"*'"
      END IF 

      IF lr_filtro.campo = ""  OR lr_filtro.campo IS NULL THEN  
        LET le_query = "SELECT emp_idempresa, emp_nomempresa ",
                       "FROM top_empresa ",
                       "WHERE suc_cod = ",g_sucursal CLIPPED,
                       " AND tip_idtipoem = 1 "
      END IF   
  
      PREPARE ext_sql22 FROM le_query
      DECLARE lc_help22 CURSOR FOR ext_sql22
      CALL l_arr_help.clear()

      FOREACH lc_help22 INTO lr_help.*
        CALL l_arr_help.appendElement()
        LET l_arr_help[l_arr_help.getLength()].* = lr_help.*
      END FOREACH

      IF l_arr_help.getLength() = 0 THEN
        CALL fgl_winmessage("Consulta","No existen Registros para ese Parametro!", "information")
      END IF
   
  END INPUT
  DISPLAY ARRAY l_arr_help TO table2.*
    ON ACTION ACCEPT
      RETURN l_arr_help[arr_curr()].codigo
  END DISPLAY
  ON ACTION CANCEL
    RETURN gr_vehiculo.emp_idempresa
  BEFORE DIALOG
    NEXT FIELD campo
END DIALOG

END FUNCTION

FUNCTION valida_placa(p)

DEFINE p CHAR(6)
DEFINE i,es_letra,es_numero,j SMALLINT
DEFINE exito BOOLEAN

LET exito = TRUE

FOR i = 1 TO 6
 IF i < 4 THEN
  LET es_letra = FALSE
  FOR j = 65 TO 90
   IF p[i] = ascii(j) THEN
    LET es_letra = TRUE
   END IF
  END FOR
  IF NOT es_letra THEN
   LET exito = FALSE
   EXIT FOR
  END IF
 ELSE
  LET es_numero= FALSE
  FOR j = 48 TO 57
   IF p[i] = ascii(j) THEN
    LET es_numero= TRUE
   END IF
  END FOR
  IF NOT es_numero THEN
   LET exito = FALSE
   EXIT FOR
  END IF
 END IF
END FOR

RETURN exito

END FUNCTION