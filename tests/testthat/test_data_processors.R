context('Testing data processing')

test_that(desc = 'testing bin_data',
          code = {

            f <- file.path(system.file(package = 'actogrammr'), 'testdata')
            d <- read_clock_lab_file(file_name = list.files(path = f, full.names = TRUE)[1])

            b <- bin_data(data = d, minutes_per_bin = 6)

            expect_is(object = b, class = 'tbl_df')
            expect_is(object = b$date, class = 'Date')
            expect_is(object = b$hour, class = 'integer')
            expect_is(object = b$bin, class = 'integer')
            expect_is(object = b$bin_act, class = 'integer')
            expect_is(object = b$bin_light, class = 'integer')

            expect_true(object = all(b$hour %in% 0:23))
            expect_true(object = all(b$bin >= 0))
            expect_true(object = all(b$bin_act >= 0, na.rm = TRUE))
            expect_true(object = all(b$bin_light >= 0))

          }
)



