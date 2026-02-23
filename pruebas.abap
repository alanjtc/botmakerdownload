CLASS ycl_cr_xml_sd_generator DEFINITION
  PUBLIC
  INHERITING FROM ycl_cr_xml_generator
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS get_multitram_description
      IMPORTING
        !it_vbap              TYPE vbap_t
        !is_vbrp              TYPE vbrp
      RETURNING
        VALUE(rv_description) TYPE string .
  PROTECTED SECTION.

    METHODS map_lines
      IMPORTING
        !is_data TYPE ty_data
      CHANGING
        !cs_data TYPE ty_map_data .
    METHODS map_header
      IMPORTING
        !is_data TYPE ty_data
      CHANGING
        !cs_data TYPE ty_map_data .
    METHODS map_tax_item
      IMPORTING
        !is_data  TYPE ty_data
        !is_vbrp  TYPE vbrp
        !iv_tabix TYPE i
      CHANGING
        !cs_data  TYPE ty_map_data .
    METHODS map_iddoc
      IMPORTING
        !is_data TYPE ty_data
      CHANGING
        !cs_data TYPE ty_map_data .
    METHODS map_sender
      IMPORTING
        !is_data TYPE ty_data
      CHANGING
        !cs_data TYPE ty_map_data .
    METHODS map_receipt
      IMPORTING
        !is_data TYPE ty_data
      CHANGING
        !cs_data TYPE ty_map_data .
    METHODS map_totals
      IMPORTING
        !is_data TYPE ty_data
      CHANGING
        !cs_data TYPE ty_map_data .
    METHODS map_aditionals
      IMPORTING
        !is_data TYPE ty_data
      CHANGING
        !cs_data TYPE ty_map_data .
    METHODS map_disc_item
      IMPORTING
        !is_data  TYPE ty_data
        !is_vbrp  TYPE vbrp
        !iv_tabix TYPE i
      CHANGING
        !cs_data  TYPE ty_map_data .

    METHODS get_data
        REDEFINITION .
    METHODS get_xml
        REDEFINITION .
    METHODS map_data
        REDEFINITION .
    METHODS map_send_data
        REDEFINITION .
  PRIVATE SECTION.

    METHODS get_atinn_number
      IMPORTING
        !iv_atnam       TYPE string
      RETURNING
        VALUE(rv_atinn) TYPE atinn .
    METHODS get_aditional_4
      IMPORTING
        !is_data              TYPE ty_data
      CHANGING
        !cv_flete             TYPE string OPTIONAL
      RETURNING
        VALUE(rv_aditional_4) TYPE string .

ENDCLASS.



CLASS YCL_CR_XML_SD_GENERATOR IMPLEMENTATION.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_CR_XML_SD_GENERATOR->GET_ADITIONAL_4
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_DATA
* | [<-->] CV_FLETE                       TYPE        STRING(optional)
* | [<-()] RV_ADITIONAL_4                 TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_aditional_4.

*-  Variables para cálculo de pesos
    DATA:
      lv_peso_neto       TYPE p DECIMALS 3,
      lv_tara_total      TYPE p DECIMALS 3,
      lv_peso_bruto      TYPE p DECIMALS 3,
      lv_flete           TYPE p DECIMALS 5,
      lt_docflow         TYPE tdt_docflow.

    rv_aditional_4 = |~~|.

*-  ================================================
*-  🟦 PASO 1: OBTENER PESO NETO
*-  ================================================
*-  El peso neto se calcula SOLO para las líneas de factura actual (VBRP)
*-  Cada VBRP apunta a una posición específica en LIPS (VGBEL + VGPOS)
*-  Esto es importante cuando hay múltiples facturas para la misma delivery

*-  Obtener todas las líneas LIPS que corresponden a esta factura
    SELECT kcntgew, kcgewei, ntgew, gewei
      INTO TABLE @DATA(lt_lips_factura)
      FROM lips
      FOR ALL ENTRIES IN @is_data-t_vbrp
      WHERE vbeln = @is_data-t_vbrp-vgbel
        AND posnr = @is_data-t_vbrp-vgpos
        AND posnr < '900000'.

    IF lt_lips_factura IS NOT INITIAL.

*-    Sumar pesos netos, cada uno con su unidad correcta convertida a KG
      lv_peso_neto = REDUCE #( INIT lv_neto TYPE p DECIMALS 3
                               FOR ls_lips IN lt_lips_factura
                               NEXT lv_neto = lv_neto + COND #(
                                 WHEN ls_lips-kcntgew IS NOT INITIAL
                                   THEN me->conv_material_unit( iv_input       = ls_lips-kcntgew
                                                                 lv_unit_input  = ls_lips-kcgewei
                                                                 lv_unit_output = 'KG' )
                                 ELSE me->conv_material_unit( iv_input       = ls_lips-ntgew
                                                               lv_unit_input  = ls_lips-gewei
                                                               lv_unit_output = 'KG' )
                               ) ).

    ENDIF.

*-  ================================================
*-  🟦 PASO 2: OBTENER TARA (PESO DE EMBALAJE)
*-  ================================================
*-  Paso 2.1: Obtener la entrega desde el primer VBRP
    DATA(lv_vbeln_delivery) = is_data-t_vbrp[ 1 ]-vgbel.

    SELECT SINGLE *
      FROM likp
      WHERE vbeln = @lv_vbeln_delivery
      INTO @DATA(ls_likp).

    IF sy-subrc = 0.

*-    Paso 2.2: Lectura de VEPO (obtener VENUM y POSNR)
*-    Obtener embalajes de todas las posiciones de esta entrega
      SELECT DISTINCT venum, posnr
        FROM vepo
        WHERE vbeln = @ls_likp-vbeln
        INTO TABLE @DATA(lt_vepo_data).

*-    Paso 2.3: Lectura de VEKP (obtener peso y unidad de tara)
      IF lt_vepo_data IS NOT INITIAL.

        SELECT venum, posnr, tarag, gewei
          FROM vekp
          FOR ALL ENTRIES IN @lt_vepo_data
          WHERE venum = @lt_vepo_data-venum
          INTO TABLE @DATA(lt_vekp_detalle).

*-      Paso 2.4: Agrupar TARA por POSNR y sumar (convertidas a KG)
        IF lt_vekp_detalle IS NOT INITIAL.

*-        Crear estructura para agrupar por POSNR
          DATA: lt_tara_agrupada TYPE TABLE OF
                  STRUCTURE { posnr TYPE vepo-posnr
                             tara_total TYPE p DECIMALS 3 },
                ls_tara_agrp TYPE STRUCTURE { posnr TYPE vepo-posnr
                                             tara_total TYPE p DECIMALS 3 }.

*-        Agrupar y sumar TARAG por POSNR
          LOOP AT lt_vekp_detalle INTO DATA(ls_vekp).
            MOVE-CORRESPONDING ls_vekp TO ls_tara_agrp.

*-          Convertir TARAG a KG
            DATA(lv_tara_kg) = COND #(
              WHEN ls_vekp-gewei NE 'KG'
                THEN me->conv_material_unit( iv_input       = ls_vekp-tarag
                                             lv_unit_input  = ls_vekp-gewei
                                             lv_unit_output = 'KG' )
              ELSE ls_vekp-tarag
            ).

*-          Buscar si POSNR ya existe en tabla agrupada
            READ TABLE lt_tara_agrupada INTO ls_tara_agrp
              WITH KEY posnr = ls_vekp-posnr.

            IF sy-subrc = 0.
*-            Sumar a la tara existente
              ls_tara_agrp-tara_total = ls_tara_agrp-tara_total + lv_tara_kg.
              MODIFY lt_tara_agrupada FROM ls_tara_agrp.
            ELSE.
*-            Crear nueva entrada para esta POSNR
              ls_tara_agrp-posnr = ls_vekp-posnr.
              ls_tara_agrp-tara_total = lv_tara_kg.
              APPEND ls_tara_agrp TO lt_tara_agrupada.
            ENDIF.
          ENDLOOP.

*-        Sumar todas las TARAG agrupadas y redondear hacia arriba (ceiling)
          lv_tara_total = 0.
          LOOP AT lt_tara_agrupada INTO ls_tara_agrp.
*-          Redondear hacia arriba cada tara de posición
            DATA(lv_tara_redondeada) = CEILING( ls_tara_agrp-tara_total ).
            lv_tara_total = lv_tara_total + lv_tara_redondeada.
          ENDLOOP.

        ENDIF.

      ENDIF.

    ENDIF.

*-  ================================================
*-  🟦 PASO 3: CALCULAR PESO BRUTO
*-  ================================================
    lv_peso_bruto = lv_peso_neto + lv_tara_total.

*-  Obtener cantidad de bultos SOLO para esta factura
*-  Se buscan en las líneas LIPS que corresponden a esta factura
    SELECT SUM( lfimg )
      INTO @DATA(lv_lfimg)
      FROM lips
      FOR ALL ENTRIES IN @is_data-t_vbrp
      WHERE vbeln = @is_data-t_vbrp-vgbel
        AND posnr = @is_data-t_vbrp-vgpos
        AND prodh = 'NDRMPACK'.

    IF sy-subrc NE 0 OR lv_lfimg IS INITIAL.
      SELECT SUM( lfimg )
        INTO @lv_lfimg
        FROM lips
        FOR ALL ENTRIES IN @is_data-t_vbrp
        WHERE vbeln = @is_data-t_vbrp-vgbel
          AND posnr = @is_data-t_vbrp-vgpos
          AND prodh = 'NDRMDRUM'.
    ENDIF.

    lv_lfimg = round( val = lv_lfimg dec = 0 ).

*-  ================================================
*-  🟦 PASO 4: CALCULAR FLETE PRORRATEDO (Si aplica)
*-  ================================================
*-  Si el parámetro CV_FLETE fue proporcionado por el llamador
*-  se calcula el flete prorrateado según el peso de esta entrega

    CHECK cv_flete IS SUPPLIED.

*-  Obtener número de entrega
    DATA(lv_delivery) = is_data-t_vbrp[ 1 ]-vgbel.

*-  Obtener flujo del documento para buscar el transporte
    CALL FUNCTION 'SD_DOCUMENT_FLOW_GET'
      EXPORTING
        iv_docnum  = lv_delivery
      IMPORTING
        et_docflow = lt_docflow.

    READ TABLE lt_docflow INTO DATA(ls_pedido) WITH KEY vbtyp_n = '8'.

    CHECK sy-subrc = 0.

*-  Obtener costo del flete desde VTTK
    SELECT SINGLE tndr_maxp
      INTO @lv_flete
      FROM vttk
      WHERE tknum = @ls_pedido-vbeln.

    CHECK sy-subrc = 0.

*-  Obtener peso total de todas las entregas en el transporte
    SELECT SUM( lk~btgew )
      INTO @DATA(lv_peso_total)
      FROM vttp AS vp
      INNER JOIN likp AS lk
        ON vp~vbeln = lk~vbeln
      WHERE vp~tknum = @ls_pedido-vbeln.

    CHECK sy-subrc = 0.

*-  Prorratear flete según el peso de esta entrega
    IF lv_peso_total > 0.
      lv_flete = ( lv_peso_bruto / lv_peso_total ) * lv_flete.
    ENDIF.

    cv_flete = |{ lv_flete DECIMALS = 2 }|.

*-  ================================================
*-  ARMADO DEL RESULTADO FINAL
*-  ================================================
*-  Formato: PESO_BRUTO~PESO_NETO~CANTIDAD_BULTOS
    rv_aditional_4 = |{ CONV i( lv_peso_bruto ) }~{ CONV i( lv_peso_neto ) }~{ CONV i( lv_lfimg ) }|.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Private Method YCL_CR_XML_SD_GENERATOR->GET_ATINN_NUMBER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IV_ATNAM                       TYPE        STRING
* | [<-()] RV_ATINN                       TYPE        ATINN
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_atinn_number.

    CALL FUNCTION 'CONVERSION_EXIT_ATINN_INPUT'
      EXPORTING
        input  = iv_atnam
      IMPORTING
        output = rv_atinn.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->GET_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        ANY
* | [<---] ES_DATA                        TYPE        TY_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_data.

    DATA:
          ls_data TYPE vbrk.

    ls_data = is_data.

    DATA(lo_xml) = NEW ycl_ec_xml_dao( ).

*-  Datos de la factura
    es_data-s_vbrk = lo_xml->get_vbrk( iv_vbeln = ls_data-vbeln ).

    CHECK es_data-s_vbrk IS NOT INITIAL.

    lo_xml->get_vbrp( EXPORTING iv_vbeln  = es_data-s_vbrk-vbeln
                      CHANGING  ct_data   = es_data-t_vbrp[] ).

    lo_xml->get_makt( EXPORTING it_vbrp  = es_data-t_vbrp[]
                       CHANGING ct_data  = es_data-t_makt[] ).

    lo_xml->get_mvke( EXPORTING it_vbrp  = es_data-t_vbrp[]
                       CHANGING ct_data  = es_data-t_mvke[] ).

    lo_xml->get_marlrg( EXPORTING it_vbrp  = es_data-t_vbrp[]
                         CHANGING ct_data  = es_data-t_marlrg[] ).

    lo_xml->get_marc( EXPORTING it_vbrp  = es_data-t_vbrp[]
                       CHANGING ct_data  = es_data-t_marc[] ).

    lo_xml->get_tvm3t( EXPORTING it_mvke  = es_data-t_mvke[]
                        CHANGING ct_data  = es_data-t_tvm3t[] ).

    lo_xml->get_tvm2t( EXPORTING it_mvke  = es_data-t_mvke[]
                        CHANGING ct_data  = es_data-t_tvm2t[] ).

    lo_xml->get_tvm4t( EXPORTING it_mvke  = es_data-t_mvke[]
                        CHANGING ct_data  = es_data-t_tvm4t[] ).

    lo_xml->get_konv( EXPORTING iv_knumv = es_data-s_vbrk-knumv
                       CHANGING ct_data  = es_data-t_konv[] ).

    lo_xml->get_cable_data( EXPORTING it_vbrp  = es_data-t_vbrp[]
                             CHANGING ct_data  = es_data-t_multitramo[] ).

    lo_xml->get_prcd_elements( EXPORTING iv_knumv = es_data-s_vbrk-knumv
                                CHANGING ct_data  = es_data-t_prcd_elements[] ).

    lo_xml->get_vbkd( EXPORTING iv_data = es_data-t_vbrp[ 1 ]-aubel
                       CHANGING cs_data = es_data-s_vbkd ).

    lo_xml->get_tvgrt( EXPORTING iv_data = es_data-t_vbrp[ 1 ]-vkgrp
                       CHANGING cs_data = es_data-s_tvgrt ).

    lo_xml->get_tinct( EXPORTING iv_data = es_data-s_vbrk-inco1
                        CHANGING cs_data = es_data-s_tinct ).

    es_data-s_likp = lo_xml->get_likp( es_data-t_vbrp[ 1 ]-vgbel ).

    lo_xml->get_lips( EXPORTING iv_data = es_data-s_likp-vbeln
                      CHANGING  ct_data = es_data-t_lips ).

    lo_xml->get_vbpa( EXPORTING iv_vbeln = es_data-s_vbrk-vbeln
                       CHANGING ct_data = es_data-t_vbpa[] ).

*-  Obtenemos los datos del cliente
*    es_data-s_kna1         = lo_xml->get_kna1( iv_kunnr = es_data-s_vbrk-kunrg ).
    es_data-s_kna1         = lo_xml->get_kna1( iv_kunnr = es_data-s_vbrk-kunag ).
    es_data-s_knvv         = lo_xml->get_knvv( iv_kunnr = es_data-s_kna1-kunnr ).
    es_data-s_but000       = lo_xml->get_but000( iv_partner = |{ es_data-s_vbrk-kunag }| ).
    es_data-s_but020       = lo_xml->get_but020( iv_partner = |{ es_data-s_vbrk-kunag }| ).
    es_data-s_dfkkbptaxnum = lo_xml->get_dfkkbptaxnum( iv_partner = |{ es_data-s_vbrk-kunag }| ).

*-  Obtenemos los datos principales
    es_data-s_t001 = lo_xml->get_t001( iv_bukrs = es_data-s_vbrk-bukrs ).

    lo_xml->get_adrc( EXPORTING iv_adrnr = es_data-s_t001-adrnr
                      CHANGING  ct_data  = es_data-t_adrc[] ).

    lo_xml->get_t001z( EXPORTING iv_bukrs = es_data-s_vbrk-bukrs
                        CHANGING ct_data  = es_data-t_t001z ).

    es_data-s_vbak = lo_xml->get_vbak( iv_vbeln = es_data-t_vbrp[ 1 ]-aubel ).

    es_data-s_t052 = lo_xml->get_t052( iv_zterm = es_data-s_vbrk-zterm ).
    es_data-s_t052u = lo_xml->get_t052u( iv_zterm = es_data-s_vbrk-zterm ).

    lo_xml->get_adrc( EXPORTING iv_adrnr = es_data-s_kna1-adrnr
                      CHANGING  ct_data  = es_data-t_adrc_rcp[] ).

    TRY.

        es_data-s_kna1_d = lo_xml->get_kna1( es_data-t_vbpa[ parvw = 'WE' ]-kunnr ).
        lo_xml->get_adrc( EXPORTING iv_adrnr = es_data-s_kna1_d-adrnr
                          CHANGING  ct_data  = es_data-t_adrc_dest[] ).

      CATCH cx_root.

    ENDTRY.

    IF es_data-t_adrc_rcp IS NOT INITIAL.

      es_data-s_adrcity = lo_xml->get_adrcity( iv_city = es_data-t_adrc_rcp[ 1 ]-city1 ).

      lo_xml->get_t005u( EXPORTING it_adrc  = es_data-t_adrc_rcp
                          CHANGING ct_data  = es_data-t_t005u ).
      lo_xml->get_t005t( EXPORTING it_adrc  = es_data-t_adrc_rcp
                          CHANGING ct_data  = es_data-t_t005t ).
    ENDIF.

    IF es_data-t_adrc_dest IS NOT INITIAL.

      lo_xml->get_t005u( EXPORTING it_adrc  = es_data-t_adrc_dest
                          CHANGING ct_data  = es_data-t_t005u_d ).

      lo_xml->get_t005t( EXPORTING it_adrc  = es_data-t_adrc_dest
                          CHANGING ct_data  = es_data-t_t005t_d ).

    ENDIF.

    es_data-s_adr6 = lo_xml->get_adr6( iv_adrnr = es_data-s_but020-addrnumber ).
    es_data-s_adr2 = lo_xml->get_adr2( iv_adrnr = es_data-s_but020-addrnumber ).


*-  Flujo del documento
    CALL FUNCTION 'SD_DOCUMENT_FLOW_GET'
      EXPORTING
        iv_docnum  = es_data-s_vbrk-vbeln
      IMPORTING
        et_docflow = es_data-t_docflow.

*-  Datos de parÃ¡metros
    SELECT SINGLE *
      FROM ytfin_usu_sec_ec
      WHERE bukrs = @es_data-s_vbrk-bukrs
        AND doctype = @es_data-s_vbrk-fkart
      INTO @es_data-s_usu_sec.


*-  Datos del monitor
    SELECT SINGLE *
      FROM ytfin_044_ec
      WHERE bukrs = @es_data-s_vbrk-bukrs
        AND vbeln = @es_data-s_vbrk-vbeln
        AND gjahr = @es_data-s_vbrk-gjahr
      INTO @es_data-s_monitor.

    CLEAR:
    ls_data.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Public Method YCL_CR_XML_SD_GENERATOR->GET_MULTITRAM_DESCRIPTION
* +-------------------------------------------------------------------------------------------------+
* | [--->] IT_VBAP                        TYPE        VBAP_T
* | [--->] IS_VBRP                        TYPE        VBRP
* | [<-()] RV_DESCRIPTION                 TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_multitram_description.

    DATA:
      lv_instance     TYPE cuib_cuobj,
      ls_instance_rec TYPE ibco2_instance_rec2.

*-  Obtenemos el nÃºmero de instancia
    TRY.

        lv_instance = |{ it_vbap[ vbeln = is_vbrp-aubel  posnr = is_vbrp-posnr ]-cuobj ALPHA = IN }|.

      CATCH cx_root.

        rv_description = is_vbrp-arktx.

        RETURN.

    ENDTRY.

*-  Obtenemos la informaciÃ³n de las instancias
    CALL FUNCTION 'CUCB_GET_SINGLE_INSTANCE'
      EXPORTING
        instance                     = lv_instance
      IMPORTING
        instance_rec                 = ls_instance_rec
      EXCEPTIONS
        invalid_instance             = 1
        instance_is_a_classification = 2
        OTHERS                       = 3.

    TRY.

*-      Concatenamos el valor encontrado
        rv_description = |{ is_vbrp-arktx } { ls_instance_rec-values[ atinn = me->get_atinn_number( ycl_ec_fe_utilities=>get_constant( iv_parameter = 'MULTITRAM_ATINN' iv_bukrs  = ytfe_c_bukrs-cr
                                                                                                                                       iv_program   = 'XML'             iv_modulo = 'FE' ) ) ]-atflv && is_vbrp-vrkme }|.

      CATCH cx_root.

        rv_description = is_vbrp-arktx.

    ENDTRY.

    CLEAR:
    lv_instance,
    ls_instance_rec.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->GET_XML
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_MAP_DATA
* | [<-()] RV_XML                         TYPE        STRING
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD get_xml.


  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->MAP_ADITIONALS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_DATA
* | [<-->] CS_DATA                        TYPE        TY_MAP_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_aditionals.

    DATA:
      lt_docflow TYPE tdt_docflow,
      lv_days    TYPE t5a4a-dlydy,
      lv_date    TYPE p0001-begda,
      lv_months  TYPE t5a4a-dlymo,
      lv_years   TYPE t5a4a-dlyyr,
      lv_duedate TYPE p0001-begda,
      lv_bulto   TYPE i.
    DATA:
      ls_adress TYPE bapiaddr3,
      lt_return TYPE TABLE OF bapiret2.

    DATA:
      lv_seguro   TYPE p DECIMALS 2,
      lv_flete    TYPE string,
      lv_subtotal TYPE p DECIMALS 2,
      lv_total    TYPE p DECIMALS 2.

*-  NÃºmero de entrega
    DATA(lv_delivery) = is_data-t_vbrp[ 1 ]-vgbel.

*-  Obtenemos el flujo del documento
    CALL FUNCTION 'SD_DOCUMENT_FLOW_GET'
      EXPORTING
        iv_docnum  = lv_delivery
      IMPORTING
        et_docflow = lt_docflow.

    DATA(lv_username) = CONV bapibname-bapibname( is_data-s_vbrk-ernam ).

    CALL FUNCTION 'BAPI_USER_GET_DETAIL'
      EXPORTING
        username = lv_username
      IMPORTING
        address  = ls_adress
      TABLES
        return   = lt_return.

*-  Adicional 1
    TRY.
        cs_data-s_header-s_aditionals-adt_1 = |{ cs_data-s_header-s_receipt-cedula && '~' &&
                                                 is_data-s_vbrk-kunag && '~' &&
                                                 cs_data-s_header-s_receipt-nombre && '~' &&
                                                 cs_data-s_header-s_receipt-direccion && '~' &&
                                                 is_data-t_adrc_rcp[ 1 ]-city1 && '~' &&
*                                                 is_data-s_vbrk-land1 && '~' &&
                                                 is_data-t_t005t[ 1 ]-landx && '~' &&
                                                 is_data-t_adrc_rcp[ 1 ]-tel_number }|.

      CATCH cx_root.

    ENDTRY.

    TRY.

*-      Adicional 2
        cs_data-s_header-s_aditionals-adt_2 = |{ is_data-s_tvgrt-bezei && '~' &&
*                                                is_data-s_vbrk-ernam && '~' &&
                                                 is_data-s_kna1-pstlz && '~' &&
*                                                 is_data-t_adrc_rcp[ 1 ]-street }|.
                                                 |{ is_data-s_kna1_d-name1 } { is_data-s_kna1_d-name2 }| && '~' &&
                                                 |{ is_data-t_adrc_dest[ 1 ]-street } { is_data-t_adrc_dest[ 1 ]-str_suppl1 } { is_data-t_adrc_dest[ 1 ]-city1 } { is_data-t_t005t_d[ 1 ]-landx }|                                                  }|.

      CATCH cx_root.

    ENDTRY.

*-  Adicional 3
    lv_date = is_data-s_vbrk-fkdat.

    lv_days = |{ is_data-s_vbrk-zterm ALPHA = OUT }|.

    CALL FUNCTION 'RP_CALC_DATE_IN_INTERVAL'
      EXPORTING
        date      = lv_date
        days      = lv_days
        months    = lv_months
        years     = lv_years
      IMPORTING
        calc_date = lv_duedate.

    cs_data-s_header-s_aditionals-adt_3 = |{ is_data-s_vbrk-waerk           && '~' &&
                                             is_data-s_t052u-text1 && '~' &&
                                             lv_duedate                      }|.


*-  BEGIN OF ABOHORQUEZ "Ajuste al flete multiorden - 10.09.2025 16:24:36
*-  Adicional 4
*    cs_data-s_header-s_aditionals-adt_4 = me->get_aditional_4( is_data ).
    cs_data-s_header-s_aditionals-adt_4 = me->get_aditional_4( EXPORTING is_data  = is_data
                                                                CHANGING cv_flete = lv_flete ).

*-  END OF ABOHORQUEZ "Ajuste al flete multiorden - 10.09.2025 16:24:36

*-  Adicional 5
    cs_data-s_header-s_aditionals-adt_5 = |{ ls_adress-fullname && '~' &&
                                             '' && '~' &&
                                             COND #( WHEN is_data-s_vbrk-land1 = 'CR'
                                                      AND ( is_data-s_vbrk-taxk1 = 'B'
                                                      OR    is_data-s_vbrk-taxk1 = 'K' )
                                                     THEN ''
                                                     ELSE me->read_text( iv_data   = is_data-s_vbrk-vbeln
                                                            iv_object = 'VBBK'
                                                            iv_id     = 'ZZ85' ) )
                                              && '~' &&
                                             is_data-s_vbrk-inco1 } { is_data-s_tinct-bezei } |.

*-  Adicional 7 (Exoneraciones)
    TRY.

        DATA(lt_lines) = me->read_text_by_line( iv_data   = is_data-t_docflow[ vbtyp_n = 'C' ]-vbeln
                                                iv_object = 'VBBK'
                                                iv_id     = 'ZZ85'  ) .
        DO lines( lt_lines ) TIMES.

*          check sy-index >= 4.
          CHECK sy-index >= 5.

          cs_data-s_header-s_aditionals-adt_7 = |{ cs_data-s_header-s_aditionals-adt_7 } { lt_lines[ sy-index ]-tdline }|.

        ENDDO.

      CATCH cx_root.

    ENDTRY.

*-  Adicional 8
    cs_data-s_header-s_aditionals-adt_8 = me->read_text( iv_data   = ycl_ec_fe_utilities=>get_constant( iv_parameter = 'ADT8_TEXT' iv_bukrs   = is_data-s_vbrk-bukrs
                                                                                                        iv_program   = 'XML'       iv_modulo  = 'FE' )
                                                         iv_object = 'TEXT'
                                                         iv_id     = 'ST' ).
*-  Adicional 9
    cs_data-s_header-s_aditionals-adt_9 = me->read_text( iv_data   = ycl_ec_fe_utilities=>get_constant( iv_parameter = 'ADT9_TEXT' iv_bukrs   = is_data-s_vbrk-bukrs
                                                                                                        iv_program   = 'XML'       iv_modulo  = 'FE' )
                                                         iv_object = 'TEXT'
                                                         iv_id     = 'ST' ).

*-  BEGIN OF ABOHORQUEZ "Adicional 10 - 04.09.2025 16:17:40
    cs_data-s_header-s_aditionals-adt_10 = COND #( WHEN is_data-s_kna1-kunnr IN ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                                   iv_modulo  = 'FE'
                                                                   iv_range   = 'ADT10_CUSTOMER'
                                                                   iv_program = 'XML' )
                                                   AND is_data-t_vbrp[ 1 ]-vrkme IN ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                                   iv_modulo  = 'FE'
                                                                   iv_range   = 'ADT10_UNIT'
                                                                   iv_program = 'XML' )
                                                  THEN ycl_ec_fe_utilities=>get_constant( iv_parameter = 'ADT10_INFO' iv_bukrs   = is_data-s_vbrk-bukrs
                                                                                          iv_program   = 'XML'       iv_modulo  = 'FE' ) ).
*-  END OF ABOHORQUEZ "Adicional 10 - 04.09.2025 16:17:40

    CHECK is_data-s_kna1-land1 <> 'CR'.

    TRY.

        DATA(lv_land) = is_data-t_lips[ 1 ]-spe_herkl.

        SELECT SINGLE landx
          FROM t005t
          WHERE land1 = @lv_land
            AND spras = @sy-langu
          INTO @DATA(lv_landx).

        cs_data-s_header-s_aditionals-adt_5 = |{ ls_adress-fullname && '~' &&
                                                 '' && '~' &&
                                                 lv_landx && '~' &&
                                                 is_data-s_vbrk-inco1 } { is_data-s_tinct-bezei } |.

      CATCH cx_root.

    ENDTRY.


*-  Si la clase de factura NO es anticipo, se calcula el seguro
    IF is_data-s_vbrk-fkart NOT IN ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                                   iv_modulo  = 'FE'
                                                                   iv_range   = 'SEGURO_ANTIC'
                                                                   iv_program = 'XML' ).
*-    Seguro
      lv_seguro = COND netwr( WHEN is_data-s_vbrk-inco1 IN ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                                                           iv_modulo  = 'FE'
                                                                                           iv_range   = 'INCO_SEGURO'
                                                                                           iv_program = 'XML' )
                              THEN is_data-s_vbrk-netwr * '0.003'
                              ELSE '0.00' ).
    ENDIF.

**-  Costo del flete
*    read table lt_docflow into data(ls_pedido) with key vbtyp_n = '8'.
*
*    if sy-subrc = 0.
*
**-    Costo del flete
*      select single
*      tndr_maxp, "Costo del flete
*      tndr_maxc, "Moneda
*      tknum
*      from vttk
*      where tknum = @ls_pedido-vbeln
*      into @data(ls_vttk).
*
*      if sy-subrc = 0.
*
**-      Entregas asociadas
*        select lk~btgew, lk~gewei, lk~vbeln, lk~ntgew
*           from vttp as vt
*           inner join likp as lk
*           on vt~vbeln = lk~vbeln
*           where vt~tknum = @ls_vttk-tknum
*           into table @data(lt_likp).
*
*        try.
*
**-          Flete
*            lv_flete  = ( ls_vttk-tndr_maxp / "Costo de flete
*                                reduce #( init lv_fl type netwr "Sumatoria de los pesos reales
*                                          for ls_likp in lt_likp
*                                          next lv_fl = lv_fl + me->conv_material_unit( iv_input       = ls_likp-btgew
*                                                                                       lv_unit_input  = ls_likp-gewei
*                                                                                       lv_unit_output = 'KG' ) ) ) *
*                              me->conv_material_unit( iv_input       = lt_likp[ vbeln = lv_delivery ]-btgew
*                                                      lv_unit_input  = lt_likp[ vbeln = lv_delivery ]-gewei
*                                                      lv_unit_output = 'KG' ). "Delivery individual
*
*            if lines( lt_likp ) > 1.
*
**-            Consultamos el total de facturas asociadas a las entregas
*              select distinct vbeln, posnr
*                from vbrp
*                for all entries in @lt_likp
*               where vgbel = @lt_likp-vbeln
*                into table @data(lt_invoices_count).
*
*              data(lv_count) = lines( lt_invoices_count ).
*
**-            Eliminamos los duplicados para validar el escenario}
*              sort lt_invoices_count by vbeln ascending.
*              delete adjacent duplicates from lt_invoices_count comparing vbeln.
*
*            endif.
*
**-          Ajustamos el flete
*            lv_flete = cond #( when lines( lt_likp ) > 1  "(when lv_count = lines( lt_invoices_count ) "Transporte con varias facturas
*                               then lv_flete
*                               else ls_vttk-tndr_maxp ). "Varias delivery para una sola factura
*
*          catch cx_root.
*
*        endtry.
*
*      endif.
*
*    endif.

*-  Si el INCOTERM se encuentra en el rango, NO se calcula el flete y el seguro
    IF is_data-s_vbrk-inco1 IN ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs  iv_modulo  = 'FE'
                                                               iv_range   = 'INCO_FLETE_SEGURO'   iv_program = 'XML' ).

      lv_seguro = lv_flete = '0.00'.

    ENDIF.
*-  Total
    lv_total  =
    cs_data-s_header-s_totals-subtotal.

    lv_subtotal = cs_data-s_header-s_totals-subtotal - lv_seguro - lv_flete.


    cs_data-s_header-s_aditionals-adt_6 = lv_subtotal && '~' &&
                                          lv_seguro   && '~' &&
                                          lv_flete    && '~' &&
                                          lv_total.

    CLEAR:
    lv_total,
    lv_subtotal,
    lv_seguro,
    lv_flete.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->MAP_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_DATA
* | [<-->] CS_DATA                        TYPE        TY_MAP_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_data.
    cs_data-s_generic-acceso = ycl_ec_fe_utilities=>get_constant( iv_parameter = 'ACCESO'
                                                                  iv_bukrs     = is_data-s_vbrk-bukrs
                                                                  iv_program   = 'XML'
                                                                  iv_modulo    = 'FE' ).

*-  LÃ­neas
    me->map_lines( EXPORTING is_data = is_data
                    CHANGING cs_data = cs_data ).

*-  Encabezado
    me->map_header( EXPORTING is_data = is_data
                     CHANGING cs_data = cs_data ).



  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->MAP_DISC_ITEM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_DATA
* | [--->] IS_VBRP                        TYPE        VBRP
* | [--->] IV_TABIX                       TYPE        I
* | [<-->] CS_DATA                        TYPE        TY_MAP_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_disc_item.
  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->MAP_HEADER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_DATA
* | [<-->] CS_DATA                        TYPE        TY_MAP_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_header.
*-  IDdoc
    me->map_iddoc( EXPORTING is_data = is_data
                    CHANGING cs_data = cs_data ).

*-  Emisor
    me->map_sender( EXPORTING is_data = is_data
                     CHANGING cs_data = cs_data ).

*-  Receptor
    me->map_receipt( EXPORTING is_data = is_data
                      CHANGING cs_data = cs_data ).

*-  Totales
    me->map_totals( EXPORTING is_data = is_data
                     CHANGING cs_data = cs_data ).
*-  Adicionales
    me->map_aditionals( EXPORTING is_data = is_data
                         CHANGING cs_data = cs_data ).
*-  Concatenamos las lÃ­neas
    me->concatenate_header( CHANGING cs_data = cs_data ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->MAP_IDDOC
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_DATA
* | [<-->] CS_DATA                        TYPE        TY_MAP_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_iddoc.
    CONSTANTS:
    lc_space TYPE c VALUE space.


*-  Fecha de documento
*    cs_data-s_header-s_iddoc-fechadoc = is_data-s_vbrk-fkdat(4)   && '-' &&
*                                        is_data-s_vbrk-fkdat+4(2) && '-' &&
*                                        is_data-s_vbrk-fkdat+6(2) && lc_space &&
*                                        is_data-s_vbrk-erzet(2)   && ':' &&
*                                        is_data-s_vbrk-erzet+2(2) && ':' &&
*                                        is_data-s_vbrk-erzet+4(2) && '.000'.
    cs_data-s_header-s_iddoc-fechadoc = |{ is_data-s_vbrk-fkdat(4)   && '-' &&
                                            is_data-s_vbrk-fkdat+4(2) && '-' &&
                                            is_data-s_vbrk-fkdat+6(2) } {
                                            is_data-s_vbrk-erzet(2)   && ':' &&
                                            is_data-s_vbrk-erzet+2(2) && ':' &&
                                            is_data-s_vbrk-erzet+4(2) && '.000' }|.
*-  Flag de envÃ­o
    cs_data-s_header-s_iddoc-enviafactura = ycl_ec_fe_utilities=>get_constant( iv_parameter = 'ENVIAFACT'
                                                                               iv_bukrs     = is_data-s_vbrk-bukrs
                                                                               iv_program   = 'XML'
                                                                               iv_modulo    = 'FE' ).
*-  Tipo de Documento
    cs_data-s_header-s_iddoc-tipodoc = COND #( WHEN is_data-s_kna1-land1 <> 'CR' AND is_data-s_vbrk-vbtyp <> 'O'
                                               THEN ycl_ec_fe_utilities=>get_constant( iv_parameter = 'TIPO_EXPO'
                                                                                       iv_bukrs     = is_data-s_vbrk-bukrs
                                                                                       iv_program   = 'XML'
                                                                                       iv_modulo    = 'FE' )
                                               ELSE ycl_ec_fe_utilities=>get_conversion( iv_modulo    = 'FE'
                                                                                         iv_program   = 'XML'
                                                                                         iv_bukrs     = is_data-s_vbrk-bukrs
                                                                                         iv_parameter = 'TIPODOC'
                                                                                         iv_fieldfrom = |{ is_data-s_vbrk-vbtyp }| ) ).

*-  CondiciÃ³n
    cs_data-s_header-s_iddoc-condicion = ycl_ec_fe_utilities=>get_conversion( iv_modulo    = 'FE'
                                                                              iv_program   = 'XML'
                                                                              iv_bukrs     = is_data-s_vbrk-bukrs
                                                                              iv_parameter = 'CONDICION'
                                                                              iv_fieldfrom = |{ is_data-s_vbrk-zterm }| ).

*-  BEGIN OF ABOHORQUEZ "4.4 Sales Implementation - 28.08.2025 15:45:35
    IF cs_data-s_header-s_iddoc-condicion IS INITIAL.

      cs_data-s_header-s_iddoc-condicion = ycl_ec_fe_utilities=>get_conversion( iv_modulo    = 'FE'
                                                                                iv_program   = 'XML'
                                                                                iv_bukrs     = is_data-s_vbrk-bukrs
                                                                                iv_parameter = 'CONDICION'
                                                                                iv_fieldfrom =
                                           COND string( WHEN is_data-s_vbak-taxk1 = ycl_ec_fe_utilities=>get_constant( iv_parameter = 'VENTA_SUCESIVA'
                                                                                                                       iv_bukrs     = is_data-s_vbrk-bukrs
                                                                                                                       iv_program   = 'XML'
                                                                                                                       iv_modulo    = 'FE' )
                                                        THEN 'SUCESIVA'
                                                        ELSE 'DEFAULT' )  ).
    ENDIF.

*-  END OF ABOHORQUEZ "4.4 Sales Implementation - 28.08.2025 15:45:35

*-  Plazo
*    cs_data-s_header-s_iddoc-plazo = is_data-s_t052-ztag1.

*- Evolutive condition ke60 - CR - EX1ROJASDA - 04.11.2025

*-  Plazo
    DATA(lv_plazo) = match( val = is_data-s_t052u-text1 regex = `\d+` ).
    IF lv_plazo IS NOT INITIAL.
      cs_data-s_header-s_iddoc-plazo = lv_plazo.
    ELSE.
      lv_plazo = '000'.
      cs_data-s_header-s_iddoc-plazo = lv_plazo.
    ENDIF.


*- END Evolutive condition ke60 - CR - EX1ROJASDA - 04.11.2025

*-  Sucursal
    cs_data-s_header-s_iddoc-sucursal = ycl_ec_fe_utilities=>get_constant( iv_parameter = 'SUCURSAL'
                                                                               iv_bukrs     = is_data-s_vbrk-bukrs
                                                                               iv_program   = 'XML'
                                                                               iv_modulo    = 'FE' ).
*-  Terminal
    cs_data-s_header-s_iddoc-terminal = ycl_ec_fe_utilities=>get_constant( iv_parameter = 'TERMINAL'
                                                                               iv_bukrs     = is_data-s_vbrk-bukrs
                                                                               iv_program   = 'XML'
                                                                               iv_modulo    = 'FE' ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->MAP_LINES
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_DATA
* | [<-->] CS_DATA                        TYPE        TY_MAP_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_lines.
    DATA:
      ls_line            TYPE ty_lines,
      lv_tabix           TYPE i,
      lv_precio_unitario TYPE p DECIMALS 5,
      lv_subtotal        TYPE p DECIMALS 5.

*-    Rangos
*-    CondiciÃ³n de precio
    DATA(lr_condpre) = ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                       iv_modulo  = 'FE'
                                                       iv_range   = 'PRECIO_KEY'
                                                       iv_program = 'XML').
*-    CondiciÃ³n de descuento
    DATA(lr_conddes) = ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                       iv_modulo  = 'FE'
                                                       iv_range   = 'DESCUENTO_KEY'
                                                       iv_program = 'XML').
*-    CondiciÃ³n de impuesto
    DATA(lr_condimp) = ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                       iv_modulo  = 'FE'
                                                       iv_range   = 'IMPUESTO_KEY'
                                                       iv_program = 'XML').

*-    CondiciÃ³n inactiva
    DATA(lr_inactive_cond) = ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                             iv_modulo  = 'FE'
                                                             iv_range   = 'INACTIVE_COND'
                                                             iv_program = 'XML').
    TRY.



*-           lÃ­neas para exoneraciÃ³n
        DATA(lt_lines) = me->read_text_by_line( iv_data   = is_data-t_docflow[ vbtyp_n = COND #( WHEN me IS INSTANCE OF ycl_cr_xml_sd_nc_generator
                                                                                                 THEN 'K'
                                                                                                 ELSE 'C' ) ]-vbeln
                                                iv_object = 'VBBK'
                                                iv_id     = 'ZZ85'  ).

      CATCH cx_root.

*-        BEGIN OF Abohorquez "Exoneraciones para NC con error - 25.08.2025 17:53:55
        TRY.
            lt_lines = me->read_text_by_line( iv_data   = is_data-t_docflow[ vbtyp_n = COND #( WHEN me IS INSTANCE OF ycl_cr_xml_sd_nc_generator
                                                                                               THEN 'H' ) ]-vbeln
                                                                             iv_object = 'VBBK'
                                                                             iv_id     = 'ZZ85'  ).
          CATCH cx_root.

        ENDTRY.
*-        END OF Abohorquez "Exoneraciones para NC con error - 25.08.2025 17:53:55

    ENDTRY.
*-    Pedidos
    SELECT *
      FROM vbak
      FOR ALL ENTRIES IN @is_data-t_vbrp
      WHERE vbeln = @is_data-t_vbrp-aubel
      INTO TABLE @DATA(lt_vbak).

*-    Documentos de venta
    SELECT *
      FROM vbap
      FOR ALL ENTRIES IN @is_data-t_vbrp
      WHERE vbeln = @is_data-t_vbrp-aubel
      INTO TABLE @DATA(lt_vbap).

    LOOP AT is_data-t_vbrp INTO DATA(ls_vbrp) WHERE pstyv NOT IN ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                                                                 iv_modulo  = 'FE'
                                                                                                 iv_range   = 'HIDE_LINE'
                                                                                                 iv_program = 'XML').

      ADD 1 TO lv_tabix.

      ls_line-linea = lv_tabix.
      CONDENSE ls_line-linea.
      ls_line-codigoproducto = |{ ls_vbrp-matnr ALPHA = OUT }|.
      TRY.

          ls_line-tipocodigoproducto = ycl_ec_fe_utilities=>get_constant( iv_parameter = 'TIPOPROD'
                                                                          iv_bukrs     = is_data-s_vbrk-bukrs
                                                                          iv_program   = 'XML'
                                                                          iv_modulo    = 'FE' ).

        CATCH cx_root.

      ENDTRY.
*        ls_line-cantidad = ls_vbrp-fklmg.
      ls_line-cantidad = |{ ls_vbrp-fkimg }|.

*        ls_line-unidad = |{ ls_vbrp-meins case = lower }|.
      ls_line-unidad = |{ ycl_ec_fe_utilities=>get_conversion( iv_modulo    = 'FE'
                                                               iv_program   = 'XML'
                                                               iv_bukrs     = is_data-s_vbrk-bukrs
                                                               iv_parameter = 'UNIDAD_MEDIDA'
                                                               iv_fieldfrom = |{ ls_vbrp-vrkme }| ) }|.
*                                                                 iv_fieldfrom = |{ ls_vbrp-meins }| ) }|.

      ls_line-unidadcomercial = ls_vbrp-vrkme.

      TRY.

          ls_line-descripcion = COND #( WHEN is_data-t_multitramo[ matnr = ls_vbrp-matnr ] IS NOT INITIAL
                                        THEN me->get_multitram_description( is_vbrp  = ls_vbrp
                                                                            it_vbap  = CONV vbap_t( lt_vbap ) )
                                        ELSE ls_vbrp-arktx ).

        CATCH cx_root.
          ls_line-descripcion = ls_vbrp-arktx.
      ENDTRY.

*        lv_precio_unitario = |{ REDUCE #( INIT lv_sum TYPE kwert
*                                            FOR ls_prcd IN is_data-t_prcd_elements
*                                          WHERE ( kschl IN lr_condpre AND kposn = ls_vbrp-posnr AND kinak NOT IN lr_inactive_cond )
*                                           NEXT lv_sum = lv_sum + ls_prcd-kwert ) / ls_vbrp-fkimg  }|.
*-  BEGIN OF Abohorquez "Exoneraciones para NC con error - 25.08.2025 17:57:35
      TRY.

          lv_precio_unitario = ls_vbrp-netwr / ls_vbrp-fkimg.

        CATCH cx_root.

      ENDTRY.
*-  END OF Abohorquez "Exoneraciones para NC con error - 25.08.2025 17:57:35


*-      Redondeamos a 3 decimales
      ls_line-preciounitario = |{ lv_precio_unitario }|.

      lv_subtotal = |{ lv_subtotal + lv_precio_unitario * ls_vbrp-fkimg }|.

*Exchange 008 - ex1rojasda

*        ls_line-decuento =  reduce #( init lv_sum type kwert
*                                      for ls_prcd in is_data-t_prcd_elements
*                                    where ( kschl in lr_conddes and kposn = ls_vbrp-posnr and kinak not in lr_inactive_cond )
*                                     next lv_sum = lv_sum + abs( ls_prcd-kwert ) ).

      ls_line-decuento = REDUCE #( INIT lv_sum TYPE kwert
                                   FOR ls_prcd IN is_data-t_prcd_elements
                                   WHERE ( kschl = 'ZZD9' AND kposn = ls_vbrp-posnr AND kinak NOT IN lr_inactive_cond )
                                   NEXT lv_sum = lv_sum + abs( ls_prcd-kwert ) ).

*        LOOP AT is_data-t_prcd_elements TRANSPORTING NO FIELDS WHERE kschl IN lr_conddes
*                                                                 AND kposn = ls_vbrp-posnr.

      LOOP AT is_data-t_prcd_elements TRANSPORTING NO FIELDS WHERE kschl = 'ZZD9'
                                                             AND kposn = ls_vbrp-posnr
                                                             AND ( kinak NOT IN lr_inactive_cond ).

        ls_line-nat_descuento = ycl_ec_fe_utilities=>get_constant( iv_parameter = 'NAT_DESCUENTO'
                                                                   iv_bukrs     = is_data-s_vbrk-bukrs
                                                                   iv_program   = 'XML'
                                                                   iv_modulo    = 'FE' ).

        EXIT.
      ENDLOOP.
*END Exchange 008 - ex1rojasda

      ls_line-es_servicio = ycl_ec_fe_utilities=>get_conversion( iv_modulo    = 'FE'
                                                                 iv_program   = 'XML'
                                                                 iv_bukrs     = is_data-s_vbrk-bukrs
                                                                 iv_parameter = 'ES_SERVICIO'
                                                                 iv_fieldfrom = |{ ls_vbrp-pstyv }| ).

      ls_line-es_servicio = COND #( WHEN ls_line-es_servicio IS INITIAL
                                    THEN me->format_amount( '0' )
                                    ELSE ls_line-es_servicio ).

      ls_line-impuesto = REDUCE #( INIT lv_sum TYPE kwert
                                    FOR ls_prcd IN is_data-t_prcd_elements
                                  WHERE ( kvsl1 IN lr_condimp AND kposn = ls_vbrp-posnr AND kinak NOT IN lr_inactive_cond )
                                   NEXT lv_sum = lv_sum + ls_prcd-kwert ).

      IF is_data-s_vbrk-land1 = 'CR' AND ( is_data-s_vbrk-taxk1 = 'B' OR is_data-s_vbrk-taxk1 = 'K' ) AND lt_lines IS NOT INITIAL.

        TRY.

            ls_line-tipodocexonera   = lt_lines[ 1 ]-tdline.
            ls_line-numdocexonera    = lt_lines[ 2 ]-tdline.
            ls_line-fechaemisexon    = lt_lines[ 3 ]-tdline.
*-            BEGIN OF ABOHORQUEZ "4.4 Sales Implementation - 28.08.2025 16:13:17
*              ls_line-nombreinstituexo = is_data-s_kna1-name1.
            ls_line-nombreinstituexo = lt_lines[ 4 ]-tdline.
*-            END OF ABOHORQUEZ "4.4 Sales Implementation - 28.08.2025 16:13:17
            ls_line-montoimpexone    =
            ls_line-impuesto         = REDUCE #( INIT lv_sum TYPE kwert
                                          FOR ls_prcd IN is_data-t_prcd_elements
                                        WHERE ( kschl = 'MWST' AND kposn = ls_vbrp-posnr AND kinak NOT IN lr_inactive_cond )
                                         NEXT lv_sum = lv_sum + abs( ls_prcd-kwert ) ).
            ls_line-tarifaexonerada  = |{ is_data-t_prcd_elements[ kschl = 'MWST' kposn = ls_vbrp-posnr ]-kbetr DECIMALS = 0 }|.

          CATCH cx_root.

        ENDTRY.
      ENDIF.

      TRY.
          ls_line-adt_1 = is_data-t_marc[ matnr = ls_vbrp-matnr ]-stawn.
        CATCH cx_root.
      ENDTRY.

      TRY.
          ls_line-adt_2 = is_data-t_tvm3t[ mvgr3 = is_data-t_mvke[ matnr = ls_vbrp-matnr ]-mvgr3 ]-bezei.
        CATCH cx_root.
      ENDTRY.

      TRY.
          ls_line-adt_3 = is_data-t_marlrg[ matnr = ls_vbrp-matnr lgreg = '41' ]-concl.
        CATCH cx_root.
      ENDTRY.

      TRY.
          ls_line-adt_4 = is_data-t_tvm4t[ mvgr4 = is_data-t_mvke[ matnr = ls_vbrp-matnr ]-mvgr4 ]-bezei && '~' &&
                          is_data-t_tvm2t[ mvgr2 = is_data-t_mvke[ matnr = ls_vbrp-matnr ]-mvgr2 ]-bezei.
        CATCH cx_root.
      ENDTRY.

      TRY.
          ls_line-adt_5 = ls_vbrp-aubel && '~' && lt_vbak[ vbeln = ls_vbrp-aubel ]-bstnk && '~' && ls_vbrp-vgbel.
        CATCH cx_root.
      ENDTRY.

      TRY.

          ls_line-codproducto = is_data-t_marlrg[ matnr = ls_vbrp-matnr lgreg = '42' ]-concl.

        CATCH cx_root.

          ls_line-codproducto =  ycl_ec_fe_utilities=>get_conversion( iv_modulo    = 'FE'
                                                                      iv_program   = 'XML'
                                                                      iv_bukrs     = is_data-s_vbrk-bukrs
                                                                      iv_parameter = 'CODCABYS'
                                                                      iv_fieldfrom = |{ ls_vbrp-matnr ALPHA = OUT }| ).
      ENDTRY.

*-      Partida Arancelaria
      IF is_data-s_kna1-land1 NE 'CR'.

        ls_line-partidaarance = ls_line-adt_1.

        CONDENSE ls_line-partidaarance NO-GAPS.

      ENDIF.

*-      Agregamos la lÃ­nea a la tabla
      APPEND me->concatenate_line( ls_line ) TO cs_data-s_generic-lineas.
      CLEAR:
      ls_line.

*-      Impuesto por detalle
      me->map_tax_item( EXPORTING is_data = is_data
                                  is_vbrp = ls_vbrp
                                  iv_tabix = lv_tabix
                         CHANGING cs_data = cs_data ).


    ENDLOOP.

*-    Subtotal
    cs_data-s_header-s_totals-subtotal = lv_subtotal.

    CLEAR:
    lv_precio_unitario,
    lv_subtotal,
    lv_tabix,
    lr_inactive_cond.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->MAP_RECEIPT
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_DATA
* | [<-->] CS_DATA                        TYPE        TY_MAP_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_receipt.

*-  Indicador de FacturaciÃ³n
    DATA(lv_ind_email) = ycl_ec_fe_utilities=>get_constant( iv_parameter = 'EMAIL_INDICATOR'
                                                            iv_bukrs     = is_data-s_vbrk-bukrs
                                                            iv_program   = 'XML'
                                                            iv_modulo    = 'FE' ).

*-  Obtenemos los correos marcados con el indicador de FE
    SELECT DISTINCT a6~smtp_addr
      FROM adrt AS at
     INNER JOIN adr6 AS a6
        ON a6~addrnumber = at~addrnumber
       AND a6~persnumber = at~persnumber
       AND a6~consnumber = at~consnumber
     WHERE at~addrnumber = @is_data-s_kna1-adrnr
       AND at~remark     = @lv_ind_email
      INTO TABLE @DATA(lt_emails).

*-  Copia de correo
    cs_data-s_header-s_receipt-copia = COND #( WHEN lt_emails IS INITIAL
                                               THEN is_data-s_adr6-smtp_addr
                                               ELSE REDUCE string( INIT lv_string TYPE string
                                                                   FOR ls_email IN lt_emails
                                                                   NEXT lv_string = |{ ls_email-smtp_addr };{ lv_string }| ) ).



*-  CÃ©dula del cliente
*      cs_data-s_header-s_receipt-cedula = is_data-s_dfkkbptaxnum-taxnum
**-  BEGIN OF ABOHORQUEZ "4.4 Sales Implementation - 28.08.2025 17:24:40
*                                              && '~' && is_data-s_kna1-bran1.
**-  END OF ABOHORQUEZ "4.4 Sales Implementation - 28.08.2025 17:24:40

**-  BEGIN OF ex1rojasda "4.4 Sales Implementation - 29.08.2025
*-  CÃ©dula del cliente
    cs_data-s_header-s_receipt-cedula = COND #( WHEN is_data-s_kna1-land1 = 'CR'
                                                THEN is_data-s_dfkkbptaxnum-taxnum && '~' && is_data-s_kna1-bran1
                                                ELSE is_data-s_dfkkbptaxnum-taxnum ).
**-  END OF ex1rojasda "4.4 Sales Implementation - 29.08.2025

    TRY.

*-      DirecciÃ³n
        cs_data-s_header-s_receipt-direccion = |{ is_data-t_adrc_rcp[ 1 ]-street } { is_data-t_adrc_rcp[ 1 ]-str_suppl1 }|.
*        cs_data-s_header-s_receipt-direccion = is_data-t_adrc_rcp[ 1 ]-street && ' ' &&
*                                               is_data-t_adrc_rcp[ 1 ]-str_suppl1 && ' ' &&
*                                               is_data-t_adrc_rcp[ 1 ]-city1 && ' ' &&
*                                               is_data-s_kna1-land1.

      CATCH cx_root.

    ENDTRY.

*-  Extranjero
    cs_data-s_header-s_receipt-extranjero = ycl_ec_fe_utilities=>get_conversion( iv_modulo    = 'FE'
                                                                                 iv_program   = 'XML'
                                                                                 iv_bukrs     = is_data-s_vbrk-bukrs
                                                                                 iv_parameter = 'EXTRANJERO'
                                                                                 iv_fieldfrom = |{ cs_data-s_header-s_iddoc-tipodoc }| ).

    IF ( cs_data-s_header-s_iddoc-tipodoc = '03' OR cs_data-s_header-s_iddoc-tipodoc = '02' ) AND is_data-s_kna1-land1 <> 'CR'.
*-    NC de exportaciÃ³n
      cs_data-s_header-s_receipt-extranjero = '1'.
    ENDIF.
*-  Nombre del cliente
*    cs_data-s_header-s_receipt-nombre = is_data-s_but000-name_org1.
    cs_data-s_header-s_receipt-nombre = |{ is_data-s_kna1-name1 } { is_data-s_kna1-name2 }|.

*-  TelÃ©fono
*    cs_data-s_header-s_receipt-telefono = is_data-t_adrc_rcp[ 1 ]-tel_number.
    cs_data-s_header-s_receipt-telefono = ' '.

*-  Tipo de identificaciÃ³n
    cs_data-s_header-s_receipt-tipoid = ycl_ec_fe_utilities=>get_conversion( iv_modulo    = 'FE'
                                                                             iv_program   = 'XML'
                                                                             iv_bukrs     = is_data-s_vbrk-bukrs
                                                                             iv_parameter = 'TIPOID_RECEP'
                                                                             iv_fieldfrom = |{ is_data-s_kna1-stcdt }| ).

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->MAP_SENDER
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_DATA
* | [<-->] CS_DATA                        TYPE        TY_MAP_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_sender.

    TRY.

*-      Actividad
*        cs_data-s_header-s_sender-actividad   = is_data-t_t001z[ party = '' ]-paval.
        cs_data-s_header-s_sender-actividad = ycl_ec_fe_utilities=>get_constant( iv_parameter = 'ACTIVIDAD'
                                                                                 iv_bukrs     = is_data-s_vbrk-bukrs
                                                                                 iv_program   = 'XML'
                                                                                 iv_modulo    = 'FE' ). "dejar constante mientras tanto

      CATCH cx_root.
    ENDTRY.

*-  Consecutivo
    cs_data-s_header-s_sender-consecutivo = is_data-s_vbrk-vbeln.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->MAP_SEND_DATA
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_MAP_DATA
* | [<-->] CS_DATA                        TYPE        YTFE_CARGAR_DOC
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_send_data.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->MAP_TAX_ITEM
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_DATA
* | [--->] IS_VBRP                        TYPE        VBRP
* | [--->] IV_TABIX                       TYPE        I
* | [<-->] CS_DATA                        TYPE        TY_MAP_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_tax_item.
    DATA:
      ls_line  TYPE ty_taxitem.

*-  CondiciÃ³n de impuesto
    DATA(lr_condimp) = ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                       iv_modulo  = 'FE'
                                                       iv_range   = 'IMPUESTO_KEY'
                                                       iv_program = 'XML').
*-  CondiciÃ³n inactiva
    DATA(lr_inactive_cond) = ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                             iv_modulo  = 'FE'
                                                             iv_range   = 'INACTIVE_COND'
                                                             iv_program = 'XML').

    LOOP AT is_data-t_prcd_elements INTO DATA(ls_prcd) WHERE kschl = 'MWST'
                                                         AND kposn = is_vbrp-posnr.

      ls_line-linea = iv_tabix.

      CONDENSE ls_line-linea.

      ls_line-codigoimpuesto = ycl_ec_fe_utilities=>get_conversion( iv_bukrs = is_data-s_vbrk-bukrs
                                                                iv_modulo    = 'FE'
                                                                iv_parameter = 'TPO_IMPUESTO'
                                                                iv_program   = 'XML'
                                                                iv_fieldfrom = |{ ls_prcd-kschl }| ).
      ls_line-monto = me->format_amount( REDUCE #( INIT lv_sum TYPE kwert
                                                                FOR ls_konv IN is_data-t_prcd_elements
                                                                WHERE ( kvsl1 IN lr_condimp
                                                                AND   kposn = is_vbrp-posnr AND kinak NOT IN lr_inactive_cond )
                                                                NEXT lv_sum = lv_sum + ls_konv-kwert ) ).
*-    Escenario de ExoneraciÃ³n
      IF    is_data-s_vbrk-land1 = 'CR'
      AND ( is_data-s_vbrk-taxk1 = 'B'
      OR    is_data-s_vbrk-taxk1 = 'K' ).

        ls_line-monto = abs( ls_prcd-kwert ).

      ENDIF.

      ls_line-tarifa = |{ ls_prcd-kbetr DECIMALS = 2 }|.

*-    Agregamos la lÃ­nea a la tabla
      APPEND me->concatenate_tax_item( ls_line ) TO cs_data-s_generic-detaimp.

      CLEAR:
      ls_line.
    ENDLOOP.


    CLEAR:
    lr_inactive_cond.

  ENDMETHOD.


* <SIGNATURE>---------------------------------------------------------------------------------------+
* | Instance Protected Method YCL_CR_XML_SD_GENERATOR->MAP_TOTALS
* +-------------------------------------------------------------------------------------------------+
* | [--->] IS_DATA                        TYPE        TY_DATA
* | [<-->] CS_DATA                        TYPE        TY_MAP_DATA
* +--------------------------------------------------------------------------------------</SIGNATURE>
  METHOD map_totals.

*-  Rangos
*-  CondiciÃ³n de precio
    DATA(lr_condpre) = ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                       iv_modulo  = 'FE'
                                                       iv_range   = 'PRECIO_KEY'
                                                       iv_program = 'XML').
*-  CondiciÃ³n de descuento
    DATA(lr_conddes) = ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                       iv_modulo  = 'FE'
                                                       iv_range   = 'DESCUENTO_KEY'
                                                       iv_program = 'XML').
*-  CondiciÃ³n de impuesto
    DATA(lr_condimp) = ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                       iv_modulo  = 'FE'
                                                       iv_range   = 'IMPUESTO_KEY'
                                                       iv_program = 'XML').

*-  CondiciÃ³n inactiva
    DATA(lr_inactive_cond) = ycl_ec_fe_utilities=>get_range( iv_bukrs   = is_data-s_vbrk-bukrs
                                                             iv_modulo  = 'FE'
                                                             iv_range   = 'INACTIVE_COND'
                                                             iv_program = 'XML').
*-  Impuestos
    cs_data-s_header-s_totals-impuestos = REDUCE #( INIT lv_sum TYPE kwert
                                                    FOR ls_prcd IN is_data-t_prcd_elements
                                                    WHERE ( kvsl1 IN lr_condimp AND kinak NOT IN lr_inactive_cond )
                                                   NEXT lv_sum = lv_sum + ls_prcd-kwert ).

*-  Escenario de ExoneraciÃ³n
    IF    is_data-s_vbrk-land1 = 'CR'
    AND ( is_data-s_vbrk-taxk1 = 'B'
    OR    is_data-s_vbrk-taxk1 = 'K' ).

      cs_data-s_header-s_totals-impuestos = REDUCE #( INIT lv_sum TYPE kwert
                                                       FOR ls_prcd IN is_data-t_prcd_elements
                                                     WHERE ( kschl = 'MWST' AND kinak NOT IN lr_inactive_cond )
                                                      NEXT lv_sum = lv_sum + abs( ls_prcd-kwert ) ).

    ENDIF.

*Exchange 008 - ex1rojasda
*-  Descuentos
*    cs_data-s_header-s_totals-descuento = reduce #( init lv_sum type kwert
*                                                    for ls_prcd in is_data-t_prcd_elements
*                                                    where ( kschl in lr_conddes and kinak not in lr_inactive_cond )
*                                                   next lv_sum = lv_sum + abs( ls_prcd-kwert ) ).

*-  Descuentos por anticipo
    cs_data-s_header-s_totals-descuento = REDUCE #( INIT lv_sum TYPE kwert
                                                    FOR ls_prcd IN is_data-t_prcd_elements
                                                    WHERE ( kschl = 'ZZD9' AND kposn <> '000000' AND kinak NOT IN lr_inactive_cond )
                                                    NEXT lv_sum = lv_sum + abs( ls_prcd-kwert ) ).
*END Exchange 008 - ex1rojasda

*-  Medio de pago
    cs_data-s_header-s_totals-mediopago =  ycl_ec_fe_utilities=>get_conversion( iv_modulo    = 'FE'
                                                                                iv_program   = 'XML'
                                                                                iv_bukrs     = is_data-s_vbrk-bukrs
                                                                                iv_parameter = 'MEDIOPAGO'
                                                                                iv_fieldfrom = |{ is_data-s_vbrk-zterm }| ).

*-  BEGIN OF ABOHORQUEZ "4.4 Sales Implementation - 28.08.2025 15:45:35

    cs_data-s_header-s_totals-mediopago = COND #( WHEN cs_data-s_header-s_totals-mediopago IS INITIAL
                                                  THEN ycl_ec_fe_utilities=>get_conversion( iv_modulo    = 'FE'
                                                                                iv_program   = 'XML'
                                                                                iv_bukrs     = is_data-s_vbrk-bukrs
                                                                                iv_parameter = 'MEDIOPAGO'
                                                                                iv_fieldfrom = 'DEFAULT' )
                                                  ELSE cs_data-s_header-s_totals-mediopago ).

*-  END OF ABOHORQUEZ "4.4 Sales Implementation - 28.08.2025 15:45:35


*-  Moneda
    cs_data-s_header-s_totals-moneda = is_data-s_vbrk-waerk.


*-  Tipo de Cambio
    cs_data-s_header-s_totals-tipocambio = is_data-s_vbrk-kurrf * 100.

  ENDMETHOD.
ENDCLASS.