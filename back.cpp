
void Println(float msg) {
	msg += 1.0f;
}

struct Vector3 {
	float x, y, z;
};

void Println(Vector3* vector) {
	Println(vector->x);
	Println(vector->y);
	Println(vector->z);
}

int main() {
	Vector3 position;
	position.x = 5;
	position.y = 6;
	position.z = 7;
	Println(&position);
	return 0;
}
