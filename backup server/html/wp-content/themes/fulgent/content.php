<?php
/**
 * The default template for displaying content
 */
?>
<div class="theme-content page-margin-top col-md-12">
	<div class="container">
		<div class="row">
			<div class="content-blog col-md-8">
				 <?php while ( have_posts() ) : the_post(); ?>
						<div class="ourblog-box">
							<div class="post-header">
								<div class="post-date-blog">
									<span class="date"><a href="<?php echo esc_url(get_day_link(get_post_time('Y'), get_post_time('m'), get_post_time('j')));?>" ><?php echo get_the_date("F j, Y "); ?></a></span>
								</div>
								<?php if ( has_post_thumbnail() ) : ?>
									<div class="image-wrapper">
										<a href="<?php echo esc_url( get_permalink() ); ?>">
											<?php the_post_thumbnail( 'full', array( 'alt' => get_the_title(), 'class' => 'img-responsive') ); ?>
										</a>
									</div>
								<?php endif; ?>
							</div>
								<div class="post-detail">
									<a href="<?php echo esc_url( get_permalink() ); ?>" class="post-title"><?php the_title(); ?> </a>
									<?php fulgent_entry_meta(); ?>  
									
									<?php 
									if(is_single()){
										the_content();
													wp_link_pages( array(
													'before'      => '<div class="page-links"><span class="page-links-title">' . __( 'Pages:', 'fulgent' ) . '</span>',
													'after'       => '</div>',
													'link_before' => '<span>',
													'link_after'  => '</span>',
												) );
									}else{
										the_excerpt();
									}
								  ?> 
								</div>
						</div>
				<?php endwhile;
					wp_reset_postdata()
				 ?>
				
				<div class="site-pagination">      
			<?php fulgent_pagination(); ?>
		</div>
			</div>  
			<?php get_sidebar(); ?>
		</div>
	</div>
</div>	
