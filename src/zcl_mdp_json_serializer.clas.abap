class ZCL_MDP_JSON_SERIALIZER definition
  public
  final
  create public .

public section.
  class ZCX_MDP_JSON_INVALID definition load .

  class-methods SERIALIZE
    importing
      !NODE type ref to ZCL_MDP_JSON_NODE
    exporting
      !JSON type STRING .
private section.
    class-methods:
      serialize_node
        importing jsonnode type ref to zcl_mdp_json_node
        exporting json     type string ,
      serialize_object
        importing jsonnode type ref to zcl_mdp_json_node
        exporting json     type string,
      serialize_array
        importing jsonnode type ref to zcl_mdp_json_node
        exporting json     type string.
*        CHANGING  offset TYPE i .
    constants: co_debug_mode type i value 1.
ENDCLASS.



CLASS ZCL_MDP_JSON_SERIALIZER IMPLEMENTATION.


method serialize.


    serialize_node(
  exporting
    jsonnode = node
  importing
    json = json
     ) .

    json = json.
  endmethod.


method serialize_array.

  endmethod.


method serialize_node.
    data l_json type string.
    data : l_index type i value 0.
    data : l_child_json type string.

    case    jsonnode->json_type.
      when  zcl_mdp_json_node=>co_json_string.
        concatenate '"' jsonnode->value '"' into l_json.
      when zcl_mdp_json_node=>co_json_number.
        l_json = jsonnode->value.
      when zcl_mdp_json_node=>co_json_false.
        l_json = 'false'.
      when zcl_mdp_json_node=>co_json_true.
        l_json = 'true'.
      when zcl_mdp_json_node=>co_json_null.
        l_json = 'null'.
      when zcl_mdp_json_node=>co_json_array.

        data : wa_array_children like line of jsonnode->array_children .


        l_json = '['.
        loop at jsonnode->array_children into wa_array_children.
          if l_index > 0.
            concatenate l_json ',' into l_json.
          endif.

          serialize_node(
            exporting
              jsonnode = wa_array_children-node
            importing
              json = l_child_json
          ) .
          concatenate l_json l_child_json into l_json.
          clear wa_array_children.
          l_index = 1.
        endloop.
        concatenate l_json ']' into l_json.
      when zcl_mdp_json_node=>co_json_object.
        data : wa_object_children like line of jsonnode->object_children .


        l_json = '{'.

        l_index = 0 .
        loop at jsonnode->object_children into wa_object_children.
          if l_index > 0.
            concatenate l_json ',' into l_json.
          endif.

          if wa_object_children-key is not initial.
            concatenate l_json '"' wa_object_children-key '":' into l_json.
          endif.


          serialize_node(
            exporting
              jsonnode = wa_object_children-node
            importing
              json = l_child_json
          ) .
          concatenate l_json l_child_json into l_json.
          clear wa_array_children.
          l_index = 1.
        endloop.
        concatenate l_json '}' into l_json.
    endcase.

    json = l_json.
  endmethod.


method serialize_object.

  endmethod.
ENDCLASS.
