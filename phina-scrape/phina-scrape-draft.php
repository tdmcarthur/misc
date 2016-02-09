<?php
/**	
 * Copyright (c) 2012 Data Committee of Occupy DC
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
 * Contact: data at occupydc dot org
 */
	
  include_once("simple_html_dom.php");

  function dcra($username, $password) {
//  $tmp_fname = tempnam('/tmp', 'COOKIE');
  $tmp_fname = 'tempCookie';

  $curl_handle = curl_init ('http://phina.ran.gob.mx/phina2/');
  // http://phina.ran.gob.mx/phina2/

  curl_setopt($curl_handle, CURLOPT_COOKIEJAR, $tmp_fname);
//  curl_setopt($curl_handle, CURLOPT_COOKIEFILE, $tmp_fname );
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);
// POSTDATA=BOL1=1&txtIDUSUARIO=usr9a1d&txtCONTRASENIA=phd309c6&btn_Entrar22=Entrar&txtRegistro=
  $login_fields = array(
    'BOL1'=>'1',
    'txtIDUSUARIO'=>'usr9a1d', // $username
    'txtCONTRASENIA'=>'phd309c6', // $password
    'btn_Entrar22'=>'Entrar',
    'txtRegistro'=>''
  );
  
  echo $tmp_fname . "\n";

//  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $login_fields);
  
  
  $uname = "usr9a1d";
  $pass = "phd309c6";

//  curl_setopt($curl_handle,CURLOPT_USERPWD,"usr9a1d:$phd309c6"); 
  curl_setopt($curl_handle, CURLOPT_USERPWD, $uname . ":" . $pass); 
  
  curl_setopt($curl_handle, CURLOPT_HEADER, 1);
  curl_setopt($curl_handle, CURLOPT_TIMEOUT, 30);
  curl_setopt($curl_handle, CURLOPT_POST, 1);
  

  $login_output = curl_exec($curl_handle);
  file_put_contents('login.html', $login_output);
  
    $first_post_string = 'POSTDATA=Nuevo=vv&txtEnviado=&txtPaamSupAct=&BOL=Consultas&sIMPRIMIR=&sCLASIF=&sCveEstado=&txt_Usuario=usr9a1d&txt_Cve_Unica=';
  $second_post_string = '&sEDO1=&txt_Folio2=&txtFecha=';
  $third_post_string = '&sMUN1=&txt_Plano2=00.000000&txtAchurada=00.000000';
  $fourth_post_string = '&txtDelimitada=&txt_NoReg=00.000000';
  $fifth_post_string = '&txtNomAnterior=&txt_Parcelada=00.000000&txt_AsentH=00.000000&txtClasificacion=&hNomClas=&txt_Reser=00.000000&txt_AsenHSt=00.000000&txtInegi=&txt_Explo=00.000000&txt_UsoComun=00.000000&txtFolioTierras=&txt_Otros=00.000000&txt_Parcelada2=0&txt_AsentH2=0&txt_Reser2=0' ;




  $curl_handle = curl_init();
  $url = 'http://phina.ran.gob.mx/phina2/Consultas.jsp';
  curl_setopt($curl_handle, CURLOPT_URL, $url);
  
  curl_setopt($curl_handle, CURLOPT_COOKIE, 'JSESSIONID=F95417F42CFB9C8F75EBAE7587A44A3C');
  // http://phina.ran.gob.mx/phina2/Sessiones
  // http://phina.ran.gob.mx/phina2/Consultas.jsp
//  curl_setopt($curl_handle, CURLOPT_COOKIEFILE, $tmp_fname);
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  curl_setopt($curl_handle, CURLOPT_FOLLOWLOCATION, true);

  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $first_post_string . $second_post_string . $third_post_string . $fourth_post_string);
  
  
//  curl_setopt($curl_handle2, CURLOPT_POSTFIELDS, $login_fields);
  
  $login_output2 = curl_exec($curl_handle);
  file_put_contents('login2.html', $login_output2);
  
  
  preg_match_all("|>Seleccionar un Estado<(.*)/select|sU",
      $login_output2,
      $dept_slice, PREG_PATTERN_ORDER);
      
  preg_match_all("|<option value=\"([0-9]*)\" >|U",
      $dept_slice[0][0],
      $dept_vals, PREG_PATTERN_ORDER);
  
  echo print_r($dept_vals);
  // Thanks to http://stackoverflow.com/questions/9816889/how-to-echo-an-array-in-php
  
  
  
  $curl_handle = curl_init();
  $url = 'http://phina.ran.gob.mx/phina2/imprime';
  curl_setopt($curl_handle, CURLOPT_URL, $url);
  curl_setopt($curl_handle, CURLOPT_COOKIE, 'JSESSIONID=D9BF2350E3FFE37140F684819CA87523');
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  
  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, 'POSTDATA=Nuevo=vv&txtEnviado=&txtPaamSupAct=&BOL=Consultas&sIMPRIMIR=&sCLASIF=&sCveEstado=&txt_Usuario=usr9a1d&txt_Cve_Unica=&cboEstado=01&sEDO1=&txt_Folio2=&txtFecha=&sMUN1=&txt_Plano2=00.000000&txtAchurada=00.000000&txtDelimitada=&txt_NoReg=00.000000&txtNomAnterior=&txt_Parcelada=00.000000&txt_AsentH=00.000000&txtClasificacion=&hNomClas=&txt_Reser=00.000000&txt_AsenHSt=00.000000&txtInegi=&txt_Explo=00.000000&txt_UsoComun=00.000000&txtFolioTierras=&txt_Otros=00.000000&txt_Parcelada2=0&txt_AsentH2=0&txt_Reser2=0');

  
  $login_output2 = curl_exec($curl_handle);
  file_put_contents('login3.html', $login_output2);  

   $curl_handle = curl_init();
  $url = 'http://phina.ran.gob.mx/phina2/imprime';
  curl_setopt($curl_handle, CURLOPT_URL, $url);
  curl_setopt($curl_handle, CURLOPT_COOKIE, 'JSESSIONID=D9BF2350E3FFE37140F684819CA87523');
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  
  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, 'POSTDATA=Nuevo=vv&txtEnviado=&txtPaamSupAct=&BOL=Consultas&sIMPRIMIR=&sCLASIF=&sCveEstado=&txt_Usuario=usr9a1d&txt_Cve_Unica=&cboEstado=01&sEDO1=&txt_Folio2=&txtFecha=&Municipio=001&sMUN1=&txt_Plano2=00.000000&txtAchurada=00.000000&txtDelimitada=&txt_NoReg=00.000000&txtNomAnterior=&txt_Parcelada=00.000000&txt_AsentH=00.000000&txtClasificacion=&hNomClas=&txt_Reser=00.000000&txt_AsenHSt=00.000000&txtInegi=&txt_Explo=00.000000&txt_UsoComun=00.000000&txtFolioTierras=&txt_Otros=00.000000&txt_Parcelada2=0&txt_AsentH2=0&txt_Reser2=0');

  
  $login_output2 = curl_exec($curl_handle);
  file_put_contents('login4.html', $login_output2); 
 
 
   $curl_handle = curl_init();
  $url = 'http://phina.ran.gob.mx/phina2/imprime';
  curl_setopt($curl_handle, CURLOPT_URL, $url);
  curl_setopt($curl_handle, CURLOPT_COOKIE, 'JSESSIONID=D9BF2350E3FFE37140F684819CA87523');
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  
  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, 'POSTDATA=Nuevo=vv&txtEnviado=&txtPaamSupAct=&BOL=Consultas&sIMPRIMIR=&sCLASIF=&sCveEstado=&txt_Usuario=usr9a1d&txt_Cve_Unica=&cboEstado=01&sEDO1=&txt_Folio2=&txtFecha=&Municipio=001&sMUN1=&txt_Plano2=00.000000&txtAchurada=00.000000&Tipo=1&txtDelimitada=&txt_NoReg=00.000000&txtNomAnterior=&txt_Parcelada=00.000000&txt_AsentH=00.000000&txtClasificacion=&hNomClas=&txt_Reser=00.000000&txt_AsenHSt=00.000000&txtInegi=&txt_Explo=00.000000&txt_UsoComun=00.000000&txtFolioTierras=&txt_Otros=00.000000&txt_Parcelada2=0&txt_AsentH2=0&txt_Reser2=0');

  
  $login_output2 = curl_exec($curl_handle);
  file_put_contents('login5.html', $login_output2); 


   $curl_handle = curl_init();
  $url = 'http://phina.ran.gob.mx/phina2/imprime';
  curl_setopt($curl_handle, CURLOPT_URL, $url);
  curl_setopt($curl_handle, CURLOPT_COOKIE, 'JSESSIONID=D9BF2350E3FFE37140F684819CA87523');
  curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);
  
  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, 'POSTDATA=Nuevo=vv&txtEnviado=&txtPaamSupAct=&BOL=Consultas&sIMPRIMIR=&sCLASIF=&sCveEstado=&txt_Usuario=usr9a1d&txt_Cve_Unica=&cboEstado=01&sEDO1=&txt_Folio2=&txtFecha=&Municipio=001&sMUN1=&txt_Plano2=00.000000&txtAchurada=00.000000&Tipo=1&txtDelimitada=&txt_NoReg=00.000000&nucleo=0114109621566816&txtNomAnterior=&txt_Parcelada=00.000000&txt_AsentH=00.000000&txtClasificacion=&hNomClas=&txt_Reser=00.000000&txt_AsenHSt=00.000000&txtInegi=&txt_Explo=00.000000&txt_UsoComun=00.000000&txtFolioTierras=&txt_Otros=00.000000&txt_Parcelada2=0&txt_AsentH2=0&txt_Reser2=0');

  
  $login_output2 = curl_exec($curl_handle);
  file_put_contents('login6.html', $login_output2); 
  
  
 
  
  
  
/**	  
  $fields = array(
    'BizEntitySearch_String'=>$search, 
    'Search'=>'Search',
    'BizEntitySearch_Type'=>'EntityName',
    'BizEntitySearch_DepthType'=>'StartsWith',
    'BizEntitySearch_EntityStatus'=>'',
    'BizEntitySearch_TradeNameStatus'=>''
  );

  curl_setopt($curl_handle, CURLOPT_POSTFIELDS, $fields);

  $query_output = curl_exec($curl_handle);
//  file_put_contents('dcra.html', $query_output);

  preg_match_all("|/BizEntity.aspx(.*)\w+(?=\")|U",
    $query_output,
    $out, PREG_PATTERN_ORDER);

  echo count($out[0]);
	  
  if (count($out[0])>20) {
	  exit('Greater than 20 results, so exit');
  }

  foreach ($out[0] as $leafurl) {

    $fullurl = "https://corp.dcra.dc.gov" . $leafurl;

    $curl_handle = curl_init ($fullurl);
    curl_setopt($curl_handle, CURLOPT_COOKIEFILE, $tmp_fname);
    curl_setopt($curl_handle, CURLOPT_RETURNTRANSFER, true);

    $leaf_output = curl_exec($curl_handle);
//    file_put_contents('dcra_corp.html', $leaf_output);
    
    $leaf_output = str_get_html($leaf_output)->plaintext;
    
//  echo file_get_html('/Users/travismcarthur/git/Occupy-data-processing/dc-campaign-finance/dcra_corp.html')->plaintext;
//    $html = file_get_html('/Users/travismcarthur/git/Occupy-data-processing/dc-campaign-finance/dcra_corp.html')->plaintext;

    $html = preg_replace("/\t/" , " ", $leaf_output);
	$html = preg_replace("/\r/" , " ", $html);
	$html = preg_replace("/\n/" , " ", $html);
    $html = preg_replace("/(         )+/" , "<SEPARATOR>", $html);
    $html = preg_replace("/<SEPARATOR>(\s)+/" , "<SEPARATOR>", $html);
    $html = preg_replace("/(<SEPARATOR>)+/" , "<SEPARATOR>", $html);

    $fp = fopen('dcra_temp_data.txt', 'a');
    fwrite($fp, $html . "\r\n");

    echo $fullurl . "\n";


  }
   */
//          echo $login_output . "\n";
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
		dcra($username, $password);
	} else {
		echo 'Invalid username, password or search query.';
	}

?>