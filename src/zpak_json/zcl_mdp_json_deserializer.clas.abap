class ZCL_MDP_JSON_DESERIALIZER definition
  public
  final
  create public .

public section.
  class ZCX_MDP_JSON_INVALID definition load .

  class-methods DESERIALIZE
    importing
      !JSON type STRING
    exporting
      !NODE type ref to ZCL_MDP_JSON_NODE
    raising
      ZCX_MDP_JSON_INVALID .
PRIVATE SECTION.
    CLASS-METHODS:
    deserialize_node
        IMPORTING json TYPE string
                  offset_before TYPE i
        EXPORTING jsonnode TYPE REF TO zcl_mdp_json_node
                  offset_after TYPE i
        RAISING zcx_mdp_json_invalid

   ,deserialize_object
        IMPORTING json TYPE string
                  offset_before TYPE i
        EXPORTING jsonnode TYPE REF TO zcl_mdp_json_node
                  offset_after TYPE i
        RAISING  zcx_mdp_json_invalid

   ,deserialize_array
        IMPORTING json TYPE string
                  offset_before TYPE i
        EXPORTING jsonnode TYPE REF TO zcl_mdp_json_node
                  offset_after TYPE i
        RAISING zcx_mdp_json_invalid .

    CONSTANTS: co_debug_mode TYPE i VALUE 0.
ENDCLASS.



CLASS ZCL_MDP_JSON_DESERIALIZER IMPLEMENTATION.


METHOD deserialize.

    DATA : l_jsonnode TYPE REF TO zcl_mdp_json_node.

    deserialize_node(
  EXPORTING
    json = json
    offset_before = 0
  IMPORTING
    jsonnode = l_jsonnode ) .

    node = l_jsonnode.
  ENDMETHOD.


METHOD deserialize_array.
    DATA l_json TYPE string.
    l_json = json.

    DATA l_offset TYPE i.
    l_offset = offset_before.

    DATA l_len TYPE i.

    DATA : l_array_jsonnode TYPE REF TO zcl_mdp_json_node.
    CREATE OBJECT l_array_jsonnode TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_array.

    IF co_debug_mode = 1.
      WRITE: / 'array:' .
    ENDIF.

    DO.

*  ] end
      DATA l_submatch TYPE string.
      FIND REGEX '\A\s*(\]|,)' IN SECTION OFFSET l_offset OF json
             MATCH OFFSET l_offset MATCH LENGTH l_len
             SUBMATCHES l_submatch.
      CASE l_submatch.
        WHEN ']'.
          l_offset = l_offset + l_len .
          offset_after = l_offset.
          EXIT.
        WHEN ','.
          l_offset = l_offset + l_len .
      ENDCASE.


      DATA : l_jsonnode TYPE REF TO zcl_mdp_json_node.


      DATA : wa_array_children TYPE zcl_mdp_json_node=>typ_array_children .

      deserialize_node( EXPORTING json = l_json offset_before = l_offset
        IMPORTING jsonnode = l_jsonnode offset_after = l_offset ).

      wa_array_children-node = l_jsonnode .

      APPEND wa_array_children TO l_array_jsonnode->array_children.

      offset_after = l_offset.

    ENDDO.
    jsonnode = l_array_jsonnode .

  ENDMETHOD.


method deserialize_node.

    data l_json type string.
    l_json = json.

    data l_offset type i.
    l_offset = offset_before.

    data l_len type i.

    data : l_jsonnode type ref to zcl_mdp_json_node.

*    find regex '\{|\[|"|\d|t|f' in section offset l_offset of json
    find regex '\{|\[|"|\d|t|f|n|-' in section offset l_offset of json
    match offset l_offset.

    case l_json+l_offset(1).
      when '{'.
        l_offset = l_offset + 1.
        deserialize_object( exporting json = l_json offset_before = l_offset importing jsonnode = l_jsonnode offset_after = l_offset ).
        jsonnode = l_jsonnode.
        offset_after = l_offset.
      when '['.
        l_offset = l_offset + 1.
        deserialize_array( exporting json = l_json offset_before = l_offset importing jsonnode = l_jsonnode offset_after = l_offset ).
        jsonnode = l_jsonnode.
        offset_after = l_offset.
      when '"'.
        data l_submatch type string.

*        find regex '"([^"]*)"' in section offset l_offset of json
*        match offset l_offset match length l_len
*        submatches l_submatch.

        find regex '"((\\.|[^\\"])*)"' in section offset l_offset of json
        match offset l_offset match length l_len
        submatches l_submatch.


        if co_debug_mode = 1.
          write: / 'string:' , l_submatch.
        endif.


        create object l_jsonnode type zcl_mdp_json_node exporting json_type = zcl_mdp_json_node=>co_json_string.
        l_jsonnode->value = l_submatch .

        offset_after = l_offset + l_len.
      when 't'.
        if l_json+l_offset(4) = 'true'.
          create object l_jsonnode type zcl_mdp_json_node exporting json_type = zcl_mdp_json_node=>co_json_true.
          l_jsonnode->value = l_json+l_offset(4).
          offset_after = l_offset + 4.

          if co_debug_mode = 1.
            write: / 'true'  .
          endif.
        else.
          raise exception type zcx_mdp_json_invalid.
        endif.

      when 'n'.
        if l_json+l_offset(4) = 'null'.
          create object l_jsonnode type zcl_mdp_json_node exporting json_type = zcl_mdp_json_node=>co_json_null.
          l_jsonnode->value = l_json+l_offset(4).
          offset_after = l_offset + 4.

          if co_debug_mode = 1.
            write: / 'null'  .
          endif.
        else.
          raise exception type zcx_mdp_json_invalid.
        endif.
      when 'f'.
        if l_json+l_offset(5) = 'false'.
          create object l_jsonnode type zcl_mdp_json_node exporting json_type = zcl_mdp_json_node=>co_json_false.
          l_jsonnode->value = l_json+l_offset(5).
          offset_after = l_offset + 5.

          if co_debug_mode = 1.
            write: / 'false'  .
          endif.
        else.
          raise exception type zcx_mdp_json_invalid.
        endif.
      when others.
        "ÐÞÕý¸ºÊý
        find regex '-(\d+|\d+\.\d+)|\d+|\d+\.\d+' in section offset l_offset of json
         match offset l_offset match length l_len.
        "ÐÞÕýÐ¡Êý
*        find regex '-|\d+|\d+\.\d+' in section offset l_offset of json
*         match offset l_offset match length l_len.
        "Ô­Ê¼
*        find regex '\d+' in section offset l_offset of json
*        match offset l_offset match length l_len.

        if co_debug_mode = 1.
          write: / 'number:'  , l_json+l_offset(l_len).
        endif.

        create object l_jsonnode type zcl_mdp_json_node exporting json_type = zcl_mdp_json_node=>co_json_number.
        l_jsonnode->value = l_json+l_offset(l_len).
        offset_after = l_offset + l_len.
    endcase.

    jsonnode = l_jsonnode.
  endmethod.


METHOD deserialize_object.
    DATA l_json TYPE string.
    l_json = json.

    DATA l_offset TYPE i.
    l_offset = offset_before.

    DATA l_len TYPE i.

    DATA : l_object_jsonnode TYPE REF TO zcl_mdp_json_node.
    CREATE OBJECT l_object_jsonnode TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_object.


    IF co_debug_mode = 1.
      WRITE: / 'object:' .
    ENDIF.

    DO.

*  } end
      DATA l_submatch TYPE string.
      FIND REGEX '\A\s*(\}|,)' IN SECTION OFFSET l_offset OF json
             MATCH OFFSET l_offset MATCH LENGTH l_len
             SUBMATCHES l_submatch.
      CASE l_submatch.
        WHEN '}'.
          l_offset = l_offset + l_len .
          offset_after = l_offset.
          EXIT.
        WHEN ','.
          l_offset = l_offset + l_len .
      ENDCASE.

* require a key
      FIND REGEX '\A\s*"([^:]*)"\s*:' IN SECTION OFFSET l_offset OF json
                        MATCH OFFSET l_offset MATCH LENGTH l_len
                        SUBMATCHES l_submatch.
      IF sy-subrc NE 0.
*  ERROR
*        RAISE EXCEPTION TYPE zcx_mdp_json_invalid.
      ENDIF.
      IF co_debug_mode = 1.
        WRITE: / 'key:' , l_submatch .
      ENDIF.
      l_offset = l_offset + l_len .

      DATA : l_jsonnode TYPE REF TO zcl_mdp_json_node.

      DATA : wa_object_children TYPE  zcl_mdp_json_node=>typ_object_children .
      wa_object_children-key = l_submatch .

      deserialize_node( EXPORTING json = l_json offset_before = l_offset
       IMPORTING jsonnode = l_jsonnode offset_after = l_offset ).

      wa_object_children-node = l_jsonnode .

      INSERT wa_object_children INTO TABLE l_object_jsonnode->object_children.

      offset_after = l_offset.

    ENDDO.
    jsonnode = l_object_jsonnode .
  ENDMETHOD.
ENDCLASS.
