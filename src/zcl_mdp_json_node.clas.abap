class ZCL_MDP_JSON_NODE definition
  public
  final
  create public .

public section.
  class ZCX_MDP_JSON_INVALID definition load .

  types:
    BEGIN OF typ_object_children,
             key  TYPE string,
             node TYPE REF TO zcl_mdp_json_node,
           END OF typ_object_children .
  types:
    BEGIN OF typ_array_children,
             node TYPE REF TO  zcl_mdp_json_node,
           END OF typ_array_children .
  types:
    tt_object_children TYPE HASHED TABLE OF zcl_mdp_json_node=>typ_object_children WITH UNIQUE KEY key .
  types:
    tt_array_children  TYPE TABLE OF zcl_mdp_json_node=>typ_array_children .

  constants CO_JSON_OBJECT type I value 02 ##NO_TEXT.
  constants CO_JSON_ARRAY type I value 03 ##NO_TEXT.
  constants CO_JSON_STRING type I value 04 ##NO_TEXT.
  constants CO_JSON_NUMBER type I value 05 ##NO_TEXT.
  constants CO_JSON_TRUE type I value 06 ##NO_TEXT.
  constants CO_JSON_FALSE type I value 07 ##NO_TEXT.
  constants CO_JSON_NULL type I value 08 ##NO_TEXT.
  data JSON_TYPE type I .
  data VALUE type STRING .
  data OBJECT_CHILDREN type TT_OBJECT_CHILDREN .
  data ARRAY_CHILDREN type TT_ARRAY_CHILDREN .

  methods CONSTRUCTOR
    importing
      !JSON_TYPE type I .
  methods OBJECT_GET_CHILD_NODE
    importing
      !KEY type STRING
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE .
  methods ARRAY_GET_CHILD_NODE
    importing
      !INDEX type I
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE .
  methods STRING_SET_VALUE
    importing
      value(VALUE) type STRING
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE .
  methods ARRAY_ADD_CHILD_NODE
    importing
      !CHILD_NODE type ref to ZCL_MDP_JSON_NODE
    returning
      value(ARRAY_NODE) type ref to ZCL_MDP_JSON_NODE .
  methods OBJECT_ADD_CHILD_NODE
    importing
      !CHILD_KEY type STRING
      !CHILD_NODE type ref to ZCL_MDP_JSON_NODE
    returning
      value(OBJECT_NODE) type ref to ZCL_MDP_JSON_NODE .
  methods SERIALIZE
    returning
      value(JSON_STRING) type STRING .
  class-methods DESERIALIZE
    importing
      !JSON type STRING
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE
    raising
      ZCX_MDP_JSON_INVALID .
  class-methods CREATE_NODE
    importing
      !JSON_TYPE type I
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE .
  class-methods CREATE_OBJECT_NODE
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE .
  class-methods CREATE_ARRAY_NODE
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE .
  class-methods CREATE_STRING_NODE
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE .
  class-methods CREATE_NUMBER_NODE
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE .
  class-methods CREATE_TRUE_NODE
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE .
  class-methods CREATE_FALSE_NODE
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE .
  class-methods CREATE_NULL_NODE
    returning
      value(NODE) type ref to ZCL_MDP_JSON_NODE .
PROTECTED SECTION.
PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_MDP_JSON_NODE IMPLEMENTATION.


METHOD array_add_child_node.

    DATA : wa_array_children TYPE zcl_mdp_json_node=>typ_array_children .

    wa_array_children-node = child_node .

    APPEND wa_array_children TO me->array_children.
    array_node = me.
  ENDMETHOD.


METHOD array_get_child_node.

    node = me->array_children[  index  ]-node .

  ENDMETHOD.


METHOD constructor.
    me->json_type = json_type.

  ENDMETHOD.


METHOD create_array_node.
    CREATE OBJECT node TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_array .
  ENDMETHOD.


METHOD create_false_node.
    CREATE OBJECT node TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_false .
  ENDMETHOD.


METHOD create_node.

*    DATA : l_json_node TYPE REF TO zcl_mdp_json_node.
    CREATE OBJECT node TYPE zcl_mdp_json_node EXPORTING json_type = json_type .

*    node = l_json_node.
  ENDMETHOD.


METHOD create_null_node.
    CREATE OBJECT node TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_null .
  ENDMETHOD.


METHOD create_number_node.
    CREATE OBJECT node TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_number .
  ENDMETHOD.


METHOD create_object_node.
    CREATE OBJECT node TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_object .
  ENDMETHOD.


METHOD create_string_node.
    CREATE OBJECT node TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_string .
  ENDMETHOD.


METHOD create_true_node.
    CREATE OBJECT node TYPE zcl_mdp_json_node EXPORTING json_type = zcl_mdp_json_node=>co_json_true .
  ENDMETHOD.


METHOD deserialize.
*DATA: l_json_node TYPE REF TO zcl_mdp_json_node.

    zcl_mdp_json_deserializer=>deserialize(
      EXPORTING json = json
      IMPORTING node = node
   ).

  ENDMETHOD.


METHOD object_add_child_node.

    DATA : wa_array_children TYPE zcl_mdp_json_node=>typ_array_children .

    wa_array_children-node = child_node .

    APPEND wa_array_children TO me->array_children.


    DATA : wa_object_children TYPE  zcl_mdp_json_node=>typ_object_children .
    wa_object_children-key = child_key .

    wa_object_children-node = child_node .

    INSERT wa_object_children INTO TABLE me->object_children.

    object_node = me.


  ENDMETHOD.


METHOD object_get_child_node.

    node = me->object_children[ key = key  ]-node .

  ENDMETHOD.


METHOD serialize.

    zcl_mdp_json_serializer=>serialize(
  EXPORTING node = me
  IMPORTING json = json_string ).

  ENDMETHOD.


METHOD string_set_value.
    me->value = value.
    node = me.
  ENDMETHOD.
ENDCLASS.
