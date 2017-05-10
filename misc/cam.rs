use std::*;

struct Cam {
    x: i32,
    y: i32,
}

impl Cam {
    fn width(&self) -> i32 {
        80
    }

    fn height(&self) -> i32 {
        25
    }

    fn translate_pos(&self, world_x: i32, world_y: i32) -> (i32, i32) {
        let w = self.width();
        let h = self.height();
        (self.x + world_x + (w / 2), self.y + world_y + (h / 2))
    }
}

fn main() {
    let c = Cam { x: 0, y: 0 };
    for i in -10..40 {
        for j in -10..12 {
            println!("{}, {}: {:?}", i, j, c.translate_pos(i, j));
        }
    }
}
