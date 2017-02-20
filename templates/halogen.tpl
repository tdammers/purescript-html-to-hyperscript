{%- macro renderNode(node, indent) %}
{% if node.tagName %}
{{ node.tagName -}}:
{% for child in node.children %}
{{ indent }}{{ renderNode(child, indent ~ '..') }}
{% endfor %}
{% else %}
{{ node }}

{% endif %}
{% endmacro -%}

{%- for node in nodes -%}
{{ renderNode(node, '') }}
{%- endfor -%}
