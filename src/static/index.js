// pull in desired CSS/SASS files
require( './styles/main.sass' );
require('font-awesome/css/font-awesome.css');


// inject bundled Elm app into div#main
var Elm = require( '../elm/Main' );
Elm.Main.embed( document.getElementById( 'main' ) );
