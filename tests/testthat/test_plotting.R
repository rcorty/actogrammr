context('Testing plotting')

test_that(desc = 'testing plot_actogram',
          code = {

            f <- file.path(system.file(package = 'actogrammr'), 'testdata')
            d <- read_clock_lab_file(file_name = list.files(path = f, full.names = TRUE)[1])

            b <- bin_data(data = d, minutes_per_bin = 6)

            p <- plot_actogram(data = b)

            expect_is(object = p, class = 'ggplot')


          }
)



