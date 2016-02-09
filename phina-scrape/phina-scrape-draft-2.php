<?php
/**	
 * Copyright (c) 2015 Travis McArthur
 * 
 * Licensed under the MIT License:
 * Permission is hereby granted, free of charge, to any person obtaining a copy of 
 * this software and associated documentation files (the "Software"), to deal in 
 * the Software without restriction, including without limitation the rights to 
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies 
 * of the Software, and to permit persons to whom the Software is furnished to do 
 * so, subject to the following conditions:
 * 	
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * Contact: 
 */
	
include_once("simple_html_dom.php");

function phina($username, $password) {
  
  $tmp_fname = tempnam('/tmp', 'COOKIE');
  
  $stdin = fopen('JSESSIONID.txt', 'r');
  $j_shesh_id = trim(fgets($stdin));
   
  // 'JSESSIONID=D9BF2350E3FFE37140F684819CA87523'



  $first_post_string = 'POSTDATA=Nuevo=vv&txtEnviado=&txtPaamSupAct=&BOL=Consultas&sIMPRIMIR=&sCLASIF=&sCveEstado=&txt_Usuario=usr9a1d&txt_Cve_Unica=';
  $second_post_string = '&sEDO1=&txt_Folio2=&txtFecha=';
  $third_post_string = '&sMUN1=&txt_Plano2=00.000000&txtAchurada=00.000000';
  $fourth_post_string = '&txtDelimitada=&txt_NoReg=00.000000';
  $fifth_post_string = '&txtNomAnterior=&txt_Parcelada=00.000000&txt_AsentH=00.000000&txtClasificacion=&hNomClas=&txt_Reser=00.000000&txt_AsenHSt=00.000000&txtInegi=&txt_Explo=00.000000&txt_UsoComun=00.000000&txtFolioTierras=&txt_Otros=00.000000&txt_Parcelada2=0&txt_AsentH2=0&txt_Reser2=0' ;


  $curl_handle = curl_init();
  $url = 'http://phina.ran.gob.mx/phina2/Consultas.jsp';
  curl_setopt($curl_handle, CURLOPT_URL, $url);
  
  curl_setopt($curl_handle, CURLOPT_COOKIE, $j_shesh_id);
  // http://phina.ran.gob.mx/phina2/Sessiones
  // http://phina.ran.gob.mx/phina2/Consultas.jsp
//  curl_setopt($curl_handle, CURLOPT_COOKIEFILE, $tmp_fname);
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
//  curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);
  
//  curl_setopt($curl_handle2, CURLOPT_POSTFIELDS, $login_fields);

  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $first_post_string . $second_post_string . $third_post_string . $fourth_post_string);
  
  $page_output = curl_exec($curl_handle);
  //file_put_contents('login2.html', $login_output2);
  
  
  preg_match_all("|>Seleccionar un Estado<(.*)/select|sU",
      $page_output,
      $estado_slice, PREG_PATTERN_ORDER);
      
  preg_match_all("|<option value=\"([0-9]*)\" >|U",
      $estado_slice[1][0],
      $estado_vals, PREG_PATTERN_ORDER);
  
  echo print_r($estado_vals);
  // Thanks to http://stackoverflow.com/questions/9816889/how-to-echo-an-array-in-php
  
  file_put_contents('final-test1.html', $page_output); 
  
  $estado_vals_final = $estado_vals[1] ;
//  $estado_vals_final = array_slice($estado_vals[1], 24);
  // If you want to start at the 13th state, put 12 above
  
  echo print_r($estado_vals_final);

// ESTADO LEVEL

  foreach ($estado_vals_final as $estado_targ) {
  
//  $muni_vals_final = array();
  $page_output = ''; 
  while (strlen($page_output) < 1000) {
  $stdin = fopen('JSESSIONID.txt', 'r');
  $j_shesh_id = trim(fgets($stdin));

  $sleepamount = rand( 10000, 250000 );
  // Wait (a random amount of) 0.01 to 0.25 seconds
  usleep($sleepamount) ;
  
  $curl_handle = curl_init();
  $url = 'http://phina.ran.gob.mx/phina2/imprime';
  curl_setopt($curl_handle, CURLOPT_URL, $url);
  
  curl_setopt($curl_handle, CURLOPT_COOKIE, $j_shesh_id);
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  
  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $first_post_string . '&cboEstado=' . $estado_targ . $second_post_string . $third_post_string . $fourth_post_string . $fifth_post_string);
  
  
  $page_output = curl_exec($curl_handle);
  
   
  preg_match_all("|select name=\"Municipio\"(.*)/select|sU",
      $page_output,
      $muni_slice, PREG_PATTERN_ORDER);
      
  preg_match_all("|<option value=\"([0-9]*)\" >|U",
      $muni_slice[1][0],
      $muni_vals, PREG_PATTERN_ORDER);
      // TODO: Not sure if I should do $muni_slice[1][0] or $muni_slice[0][0]
      
     file_put_contents('final-test2.html', $page_output); 
     
  $muni_vals_final = $muni_vals[1];
  }
  
 // MUNICIPIO LEVEL 
 
 foreach ($muni_vals_final as $muni_targ) { 

//  $tipo_vals_final = array();

  $page_output = '';
  while (strlen($page_output) < 1000) {
  $sleepamount = rand( 10000, 250000 );
  usleep($sleepamount) ;
  
  $curl_handle = curl_init();
  $url = 'http://phina.ran.gob.mx/phina2/imprime';
  curl_setopt($curl_handle, CURLOPT_URL, $url);
  curl_setopt($curl_handle, CURLOPT_COOKIE, $j_shesh_id);
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  

  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $first_post_string . '&cboEstado=' . $estado_targ . $second_post_string . '&Municipio=' . $muni_targ . $third_post_string . $fourth_post_string . $fifth_post_string);
  
    $page_output = curl_exec($curl_handle);
   
  preg_match_all("|select name=\"Tipo\"(.*)/select|sU",
      $page_output,
      $tipo_slice, PREG_PATTERN_ORDER);
      
  preg_match_all("|<option value=\"([0-9]*)\" >|U",
      $tipo_slice[1][0],
      $tipo_vals, PREG_PATTERN_ORDER);

 file_put_contents('final-test3.html', $page_output); 
 
 $tipo_vals_final = $tipo_vals[1];
 }

 // TIPO LEVEL 
 
 foreach ($tipo_vals_final as $tipo_targ) { 
 
//  $nucleo_vals_final = array();

//  while (count($nucleo_vals_final)==0) {
  $page_output = ''; 
  while (strlen($page_output) < 1000) {
  $sleepamount = rand( 10000, 250000 );
  usleep($sleepamount)   ;
  
  $curl_handle = curl_init();
  $url = 'http://phina.ran.gob.mx/phina2/imprime';
  curl_setopt($curl_handle, CURLOPT_URL, $url);
  curl_setopt($curl_handle, CURLOPT_COOKIE, $j_shesh_id);
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  

  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $first_post_string . '&cboEstado=' . $estado_targ . $second_post_string . '&Municipio=' . $muni_targ . $third_post_string . '&Tipo=' . $tipo_targ . $fourth_post_string . $fifth_post_string);
  
    $page_output = curl_exec($curl_handle);
   
  preg_match_all("|select name=\"nucleo\"(.*)/select|sU",
      $page_output,
      $nucleo_slice, PREG_PATTERN_ORDER);
      
  preg_match_all("|<option value=\"([0-9]*)\">|U",
      $nucleo_slice[1][0],
      $nucleo_vals, PREG_PATTERN_ORDER);
      
  file_put_contents('final-test4.html', $page_output); 
  
  $nucleo_vals_final = $nucleo_vals[1];
  }

// NUCLEO LEVEL 
 
 foreach ($nucleo_vals_final as $nucleo_targ) { 
  
  $page_output = '';
  while (strlen($page_output) < 1000) {
  $sleepamount = rand( 10000, 250000 );
  usleep($sleepamount) ;
  
  $curl_handle = curl_init();
  $url = 'http://phina.ran.gob.mx/phina2/imprime';
  curl_setopt($curl_handle, CURLOPT_URL, $url);
  curl_setopt($curl_handle, CURLOPT_COOKIE, $j_shesh_id);
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  

//  echo print_r($first_post_string . '&cboEstado=' . $estado_targ . $second_post_string . '&Municipio=' . $muni_targ . $third_post_string . '&Tipo=' . $tipo_targ . $fourth_post_string . '&nucleo=' . $nucleo_targ . $fifth_post_string) ;

  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $first_post_string . '&cboEstado=' . $estado_targ . $second_post_string . '&Municipio=' . $muni_targ . $third_post_string . '&Tipo=' . $tipo_targ . $fourth_post_string . '&nucleo=' . $nucleo_targ . $fifth_post_string);
  
    $page_output = curl_exec($curl_handle);
   
   file_put_contents('Dec-21-run/' . $nucleo_targ . 'Dec-21-run.html', $page_output); 
   
//   $filename = 'Dec-21-run/' . $nucleo_targ . 'Dec-21-run.html' ;
//   $html_filesize = filesize($filename);
   }
   
   
   }
//   break 3 ;
   }
   }
   }
   
   



}

	
//	echo 'Enter username:';
	$stdin = fopen('user.txt', 'r');
	$username = trim(fgets($stdin));
	
//	echo 'Enter password:';
	$stdin = fopen('pw.txt', 'r');
	$password = trim(fgets($stdin));
	
//	echo 'Enter search string:';
	$stdin = fopen('phinaQuery.txt', 'r');
//	$search = trim(fgets($stdin));
	
	if ($username && $password) {
		phina($username, $password);
	} else {
		echo 'Invalid username, password or search query.';
	}

?>