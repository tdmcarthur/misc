<?php


$html_page = file_get_contents('/Users/travismcarthur/git/misc/phina-scrape/Dec-21-run/0114109621566945Dec-21-run.html');
  
  function convert_accent($string)
{
//    return htmlspecialchars_decode(htmlentities(utf8_decode($string)));
//    return htmlspecialchars(utf8_decode($string));
//    return html_entity_decode(utf8_decode($string));
     return $string;
}


//echo $html_page ;

$html_page_converted = convert_accent($html_page);

//file_put_contents('Dec-21-run/test_converted.html', $html_page_converted); 


?>