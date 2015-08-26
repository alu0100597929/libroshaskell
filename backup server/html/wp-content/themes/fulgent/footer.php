  <!--Footer start-->
        <footer class="footer-css">
			<?php if (is_active_sidebar('footer-1') || is_active_sidebar('footer-2') || is_active_sidebar('footer-3') || is_active_sidebar('footer-4')) { ?>
            <div class="container">
                <div class="row">
                    <?php if (is_active_sidebar('footer-1') || is_active_sidebar('footer-2') || is_active_sidebar('footer-3') || is_active_sidebar('footer-4')) { ?>
                    <div class="col-md-12 footer-row2 main-footerbar">
                        <div class="row">
							<div class="col-md-3 col-sm-6">
								<?php if (is_active_sidebar('footer-1')) { ?> 
									<div class="footer-widget">
										<?php dynamic_sidebar('footer-1'); ?>
									</div>
								<?php } ?>
							</div>    
							<div class="col-md-3 col-sm-6">    
								<?php if (is_active_sidebar('footer-2')) { ?> 
									<div class="footer-widget">
										<?php dynamic_sidebar('footer-2'); ?>
									</div>
								<?php } ?>
							</div>  
							<div class="col-md-3 col-sm-6">    
								<?php if (is_active_sidebar('footer-3')) { ?> 
									<div class="footer-widget">
										<?php dynamic_sidebar('footer-3'); ?>
									</div>
								<?php } ?>
							</div> 
							<div class="col-md-3 col-sm-6">    
								<?php if (is_active_sidebar('footer-4')) { ?> 
									<div class="footer-widget">
										<?php dynamic_sidebar('footer-4'); ?>
									</div>
								<?php } ?>
							</div>
						</div>
                    </div>
					<?php } ?>
                </div>
            </div>
			<?php } ?>
			
            <div class="col-md-12 footer-row3">
                <div class="container">
                    <div class="row">
                        <div class="col-md-6 col-sm-6 copyright-text">
                            <p>
                             <?php
		 $fulgent_copyright_check = get_theme_mod( 'copyright_url_setting' );
				if( $fulgent_copyright_check != '' ) {
					 echo esc_html( get_theme_mod('copyright_url_setting', '') );   } 
			 printf( __( 'Powered by %1$s and %2$s ', 'fulgent' ), '<a href="http://wordpress.org" target="_blank">WordPress</a>', '<a href="http://fruitthemes.com/wordpress-themes/fulgent" target="_blank">Fulgent</a>' ); 
			
			 ?>
                            </p>
                        </div>
                        <div class="col-md-6 col-sm-6 back-to-top">
                            <a href="javascript:void(0);" class="go-top">
                                <?php _e('go to top','fulgent'); ?>
                                <i class="fa fa-caret-up"></i>
                            </a>
                        </div>
                    </div>
                </div>
            </div>
			
		</footer>
        <!--Footer end-->

<?php wp_footer(); ?>
</body>
</html>
