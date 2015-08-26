<?php
/*
 * Template Name: Home Page
 */
get_header();
?>
	<div class="container top-section">
    <?php 
		$fulgent_banner_title1 = get_theme_mod( 'fulgent_banner_title1' );
		$fulgent_banner_title2 = get_theme_mod( 'fulgent_banner_title2' );
		$fulgent_banner_title3 = get_theme_mod( 'fulgent_banner_title3' );
		$fulgent_banner_button = get_theme_mod( 'fulgent_banner_button' );
		$fulgent_banner_link = get_theme_mod( 'fulgent_banner_link' );
	?>
		<div class="banner-text col-md-12">
			<?php echo "<p class='caption-line1'>".esc_attr($fulgent_banner_title1)."</p>"; ?>
			<?php echo "<h1>".esc_attr($fulgent_banner_title2)."</h1>"; ?>
			<?php echo "<p class='caption-line2'>".esc_attr($fulgent_banner_title3)."</p>"; ?>
			<?php if(!empty($fulgent_banner_button)) {?>
			<div class="theme-button">
				<a href="<?php echo esc_url($fulgent_banner_link);?>" class="click-down transparent-btn"><?php echo esc_attr($fulgent_banner_button); ?></a>
			</div>    
			<?php } ?>
		</div>  
		<div class="arrow-down text-center col-md-12">
			<a class="arrow-up-click" href="javascript:void(0);"> 
				<span class="circle-rotate spin-div"></span>
				<i class="fa fa-chevron-down"></i>
			</a>
		</div>
	</div>
</div>	
   <section class="section-main col-md-12">
        <?php 
			$fulgent_about_us_title = get_theme_mod( 'fulgent_about_title' );
			$fulgent_about_us_info = get_theme_mod( 'fulgent_aboutus_info' ); 
			$fulgent_about_us_left_img = get_theme_mod( 'fulgent_aboutus_image_bg' );
			$fulgent_about_us_right_text = get_theme_mod( 'fulgent_aboutus_right_text' ); 
			
			if(!empty($fulgent_about_us_title) || 
			!empty($fulgent_about_us_info) || 
			!empty($fulgent_about_us_right_img) || 
			!empty($fulgent_about_us_left_text)) {
		?>
			<!-- about-->
            <div class="section-row about-secton">
                <div class="container">
                    <div class="row">
						<?php if(!empty($fulgent_about_us_title) || !empty($fulgent_about_us_info)) {?>
							<div class="col-md-12 theme-heading">
								<?php if(!empty($fulgent_about_us_title)) { ?>
									<h2 class="main-title">
										<?php echo "<span>".esc_attr( get_theme_mod('fulgent_about_title', '') )."</span>";  ?>
									</h2>
									<div class="icon-border"><span class="fa fa-sun-o spin-div"></span></div>
								<?php } ?>
								<?php if(!empty($fulgent_about_us_info)) { ?>
									<p class="title-caption">
                                    	<?php echo "<span>".esc_attr( get_theme_mod('fulgent_aboutus_info', '') )."</span>";  ?>
									</p>
								<?php } ?>
							</div>
						<?php }
								if(!empty($fulgent_about_us_left_img) || 
									!empty($fulgent_about_us_right_text)) {	
						 ?>
                        <div class="col-md-12 main-objective">
                            <div class="row">
								<div class="<?php echo $fulgent_about_us_left_img ? 'col-md-6' : 'col-md-12' ?> col-sm-12">
									<?php echo wpautop($fulgent_about_us_right_text); ?>
                                </div>
								<?php if(!empty($fulgent_about_us_left_img)) { ?>
										<div class="col-md-6 col-sm-12 objective-right">
                                            <img src="<?php echo esc_url( get_theme_mod( 'fulgent_aboutus_image_bg' ) ); ?>" alt="<?php _e('What You Get','fulgent'); ?>">
										</div>
								<?php } ?>
                            </div>    
                        </div>
						<?php } ?>
                        
                    </div>
                </div>
            </div>
		<?php } ?>
            <?php
				$fulgent_mainmoto_title = get_theme_mod( 'fulgent_mainmototitle' ); 
				$fulgent_mainmoto_info = get_theme_mod( 'fulgent_mainmotoinfo' );
			    if(!empty($fulgent_mainmoto_title) || !empty($fulgent_mainmoto_info) ) { ?>
				<div class="section-row moto-section parallax-fulgent" data-type="background" data-speed="2">
					<div class="blur-slider"></div>
					<div class="container">
						<div class="moto-content">
                            <?php echo "<h2 class='moto-title'>".esc_attr( get_theme_mod('fulgent_mainmototitle', '') )."</h2>";  ?>
							<?php echo wpautop($fulgent_mainmoto_info); ?>
						</div>
					</div>
				</div>
			<?php  } ?>

            <div class="section-row ourblog-section">
                <div class="container">
                    <div class="row">
							<div class="col-md-12 theme-heading">
									<h2 class="main-title">
                                        <span>
    	                                    <?php $blog_check = get_theme_mod( 'fulgent_blog_title' );
												if(!empty($blog_check)) {
													 echo esc_attr( get_theme_mod('fulgent_blog_title', '') ); 
												 } else { 
													echo __('Our Blog', 'fulgent'); 
											 } ?>
                                        </span>
									</h2>
									<div class="icon-border"><span class="fa fa-sun-o spin-div"></span></div>
							</div>
                        <div class="col-md-12 our-blog">
                            <div class="row">
							<?php
								$fulgent_blogcategory=get_theme_mod('fulgent_blogcategory');
									$fulgent_args = array(
										'ignore_sticky_posts' => '1',
										'meta_query' => array(
											array(
												'key' => '_thumbnail_id',
												'compare' => 'EXISTS'
											),
										)
									);
									if(!empty($fulgent_blogcategory))
										$fulgent_args['cat']=absint($fulgent_blogcategory);
										$fulgent_query = new WP_Query($fulgent_args);
										if ($fulgent_query->have_posts()) : while ($fulgent_query->have_posts()) : $fulgent_query->the_post();
							?>
							<div class="col-md-6 col-sm-6">
								<div class="ourblog-box">
									<div class="post-header">
										<div class="post-date-blog">
											<span class="date"><a href="<?php echo esc_url(get_day_link(get_post_time('Y'), get_post_time('m'), get_post_time('j'))); ?>"><?php echo get_the_date("F j, Y "); ?></a></span>
										</div>
										<?php if ( has_post_thumbnail() ) : ?>
										<div class="image-wrapper">
											<div class="blur-img"></div>
											<a href="<?php echo esc_url( get_permalink() ); ?>">
												<?php the_post_thumbnail( 'fulgent-home-blog-image', array( 'alt' => get_the_title(), 'class' => 'img-responsive') ); ?>
											</a>
										</div>
										<?php endif; ?>
									</div>
									<div class="post-detail">
										<a class="post-title" href="<?php echo esc_url( get_permalink() ) ; ?>"><?php the_title(); ?></a>
										<div class="post-meta">
                                               <ul>
                                                    <li class="post-author"><span><?php  echo __('By', 'fulgent');?></span><a href="<?php echo esc_url(get_author_posts_url(get_the_author_meta('ID'))); ?>"> <?php the_author(); ?> </a></li>
                                                    <li class="post-category"><span><?php  echo __('In', 'fulgent');?></span><?php the_category(' ,','');?></li>
                                                </ul>     
                                            </div>
									</div>
								</div>
							</div>
                             <?php endwhile; endif; ?> 
                             <?php wp_reset_postdata(); ?>
						    </div>
                        </div>
                    </div>
                </div>    
            </div>

        </section>
      
<?php get_footer(); ?>
