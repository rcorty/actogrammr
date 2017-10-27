context('Testing data reading')

test_that(desc = 'read_clock_lab_file',
          code = {

            f <- file.path(system.file(package = 'actogrammr'), 'testdata')
            d <- read_clock_lab_file(file_name = list.files(path = f, full.names = TRUE)[1])

            expect_is(object = d, class = 'tbl_df')
            expect_is(object = d$date, class = 'Date')
            expect_is(object = d$hour, class = 'integer')
            expect_is(object = d$min, class = 'integer')
            expect_is(object = d$act, class = 'integer')
            expect_is(object = d$light, class = 'integer')

            expect_true(object = all(d$hour %in% 0:23))
            expect_true(object = all(d$min %in% 0:60))
            expect_true(object = all(d$act >= 0, na.rm = TRUE))
            expect_true(object = all(d$light >= 0))

          }
)



test_that(desc = 'read_clock_lab_files',
          code = {

            f <- file.path(system.file(package = 'actogrammr'), 'testdata')
            d <- read_clock_lab_files(file_names = list.files(path = f, full.names = TRUE))

            expect_is(object = d, class = 'tbl_df')
            expect_is(object = d$date, class = 'Date')
            expect_is(object = d$hour, class = 'integer')
            expect_is(object = d$min, class = 'integer')
            expect_is(object = d$act, class = 'integer')
            expect_is(object = d$light, class = 'integer')

            expect_true(object = all(d$hour %in% 0:23))
            expect_true(object = all(d$min %in% 0:60))
            expect_true(object = all(d$act >= 0, na.rm = TRUE))
            expect_true(object = all(d$light >= 0))

          }
)


