<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>{{title}}</title>
</head>
<body>
<h1>{{title}}</h1>

{{#description}}
<p>{{{description}}}</p>
{{/description}}

<img src="./{{filename_without_extension}}.dfd.png">
<a href="./{{filename_without_extension}}.seq.png">Sequence Diagram</a>

<ol>
{{#flows}}
  <li>
    {{#operation}}
    <p><strong>{{{operation}}}</strong></p>
    {{/operation}}

    {{#data}}
    <p><strong>Data:</strong> {{{data}}}</p>
    {{/data}}

    {{#description}}
    <p>{{{description}}}</p>
    {{/description}}
  </li>
{{/flows}}
</ol>

<h2>Threats</h2>

{{#threats}}
<p>{{{threats}}}</p>
{{/threats}}

</body>
</html>
