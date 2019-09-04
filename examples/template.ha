<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <title>{{title}}</title>

  <style>
  li h2 {
    font-size: 125%;
  }
  ul ul {
    list-style-type: upper-roman;
  }
  </style>
</head>
<body>
<h1>{{title}}</h1>

{{#description}}
{{#markdown}}description{{/markdown}}
{{/description}}

<img src="./{{filename_without_extension}}.dfd.png">
<a href="./{{filename_without_extension}}.seq.png">Sequence Diagram</a>

<ol>
{{#flows}}
  <li>
    {{#operation}}
    <h2>{{{operation}}}</h2>
    {{/operation}}

    <ul>
    {{#data}}
      <li>{{&value}}</li>
    {{/data}}
    </ul>

    {{#description}}
    <p>{{#html_linebreaks}}description{{/html_linebreaks}}</p>
    {{/description}}
  </li>
{{/flows}}
</ol>

<h2>Threats</h2>

<ul>
{{#threats}}
<li><code>{{value}}</code></li>
{{/threats}}
</ul>

</body>
</html>
