<?php
/**
 * 404 pages (not found)
*/
get_header(); ?>
    <section class="section-main col-md-12 top-section">
		<div class="content-page">
			<div class="theme-content page-margin-top col-md-12">
				<div class="container">
					<div id="primary" class="content-area-404">
						<section>
							<h1 class="page-title-404"><?php _e( 'Oops! That page can&rsquo;t be found.', 'fulgent' ); ?></h1>
							<div class="page-content">
								<p><?php _e( 'It looks like nothing was found at this location. Maybe try a search?', 'fulgent' ); ?></p>
								<?php get_search_form(); ?>
							</div><!-- .page-content -->
						</section><!-- .error-404 -->
					</div><!-- .content-area -->
				</div>
			</div>	
		  
		 </div>
		<!--end about-->
    </section>
    
<?php get_footer(); ?>
