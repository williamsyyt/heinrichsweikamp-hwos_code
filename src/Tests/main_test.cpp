//////////////////////////////////////////////////////////////////////////////
/// main_test.cpp
/// Launch all defined GoogleTest unit tests defined.
/// Copyright (c) 2012-2015, JD Gascuel, HeinrichsWeikamp, all right reserved.
//////////////////////////////////////////////////////////////////////////////
//  HISTORY
// 2015-05-23 jDG: Rewrite compass testing, to allow reducing code size.

#include <gtest/gtest.h>

int main(int argc, char *argv[])
{
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}

