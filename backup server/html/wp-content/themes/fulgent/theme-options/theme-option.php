<?php
function fulgent_options_init(){
 register_setting( 'fulgent_options', 'fulgent_theme_options','fulgent_options_validate');
} 
add_action( 'admin_init', 'fulgent_options_init' );
function fulgent_framework_load_scripts(){
	wp_enqueue_media();
	wp_enqueue_style( 'fulgent_framework', get_template_directory_uri(). '/theme-options/css/theme-option_framework.css' ,false, '1.0.0');	
	wp_localize_script('options-custom', 'admin_url', admin_url('admin-ajax.php'));
}
add_action( 'admin_enqueue_scripts', 'fulgent_framework_load_scripts' );
function fulgent_framework_menu_settings() {
	$fulgent_menu = array(
				'page_title' => __( 'fulgent Options', 'fulgent_framework'),
				'menu_title' => __('Fulgent Pro Features', 'fulgent_framework'),
				'capability' => 'edit_theme_options',
				'menu_slug' => 'fulgent_framework',
				'callback' => 'fulgent_framework_page'
				);
	return apply_filters( 'fulgent_framework_menu', $fulgent_menu );
}
add_action( 'admin_menu', 'fulgent_options_add_page' ); 
function fulgent_options_add_page() {
	$fulgent_menu = fulgent_framework_menu_settings();
   	add_theme_page($fulgent_menu['page_title'],$fulgent_menu['menu_title'],$fulgent_menu['capability'],$fulgent_menu['menu_slug'],$fulgent_menu['callback']);
} 
function fulgent_framework_page(){ 
		global $select_options; 
		if ( ! isset( $_REQUEST['settings-updated'] ) ) 
		$_REQUEST['settings-updated'] = false;		

?>
<div class="theme-option-themes">
	<form method="post" action="options.php" id="form-option" class="theme_option_ft">
  <div class="theme-option-header">
    <div class="logo">
       <?php
		$fulgent_image=get_template_directory_uri().'/theme-options/images/logo.png';
		echo "<a href='http://fruitthemes.com' target='_blank'><img src='".$fulgent_image."' alt='fruitthemes' /></a>";
		?>
    </div>
  </div>
  <div class="theme-option-details">
    <div class="theme-option-options">
      <div class="right-box">
        <div class="nav-tab-wrapper">
          <ul>
            <li><a id="options-group-1-tab" class="nav-tab basicsettings-tab" title="<?php _e('PRO Theme Features','fulgent'); ?>" href="#options-group-1"><?php _e('PRO Theme Features','fulgent'); ?></a></li>
          </ul>  
        </div>
      </div>
      <div class="right-box-bg"></div>
      <div class="postbox left-box"> 
        <!--======================== F I N A L - - T H E M E - - O P T I O N ===================-->
          <?php settings_fields( 'fulgent_options' );  
		$fulgent_options = get_option( 'fulgent_theme_options' );
		 ?>
          <div id="options-group-1" class="group theme-option-inner-tabs"> 
				<div class="fulgent-pro-header">
              <h2 class="fulgent-pro-logo">Fulgent PRO</h2>
              <a href="http://fruitthemes.com/wordpress-themes/fulgent" target="_blank">
					<img src="<?php echo get_template_directory_uri(); ?>/theme-options/images/fulgent-buy-now.png" class="fulgent-pro-buynow" /></a>  
              </div>
          	<img src="<?php echo get_template_directory_uri(); ?>/theme-options/images/fulgent_pro.png" class="fulgent-pro-image" />
		  </div>
      </div>
     </div>
	</div>
   </form>    
</div>
<?php } ?>